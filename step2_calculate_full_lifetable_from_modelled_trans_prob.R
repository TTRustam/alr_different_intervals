# ----------------------------------------------------------------- #
# source function
source("functions.R")

# load database
load("Results/extr_self.RData")
load("Results/extr_chron.RData")
load("Results/extr_gali.RData")



extr_chron <- extr_chron %>% 
  mutate(adjusted = "Raw",
         type = "chronic")
extr_gali <- extr_gali%>% 
  mutate(adjusted = "Raw",
         type = "gali")
extr_self <- extr_self%>% 
  mutate(adjusted = "Raw",
         type = "self_rated")

all <- extr_chron %>% 
  full_join(extr_gali) %>% 
  full_join(extr_self) %>% 
  pivot_longer(-c(sex, time, age, adjusted, type),
               names_to  = "var",
               values_to = "p") %>% 
  mutate(from = str_sub(var, 1, 1),
         to = str_sub(var, 2)) %>% 
  dplyr::select(-var) %>% 
  rename(year = time)


# load("Results/final_share.RData")
# load("Results/final_adj_raw.RData")
# library(tidyverse)
# 
# 
# 
# share <- final_share %>% 
#   # dplyr::select(-var) %>% 
#   mutate(adjusted = "Adjusted") %>% 
#   full_join(all) %>%
#   filter(age > 20) %>%
#   mutate(panel = "SHARE") %>% 
#   full_join(final) %>%
#   mutate(panel = ifelse(is.na(panel), "SILC", panel)) %>% 
#   filter(!is.na(p))
# 
# 
# save(share, file = "Results/compare_share_silc.RData")


# share %>%
#   filter(to == "D") %>% 
#   unite("trans", c(from, to), sep = "") %>% 
#   unite("data", c(trans, panel), sep = "_") %>% 
#   filter(year %in% c(2013, 2015, 2017)) %>% 
#   filter(sex == "female") %>% 
#   ggplot(aes(x = age, y = p, color = data, lty = adjusted)) + 
#   geom_line() + 
#   facet_wrap(type ~ year) + 
#   theme_minimal() + 
#   theme(legend.position = "bottom")
  

# load("Results/transition_probs_emp.RData")

# calculate the lifetable for modelled transition probabilities
# NOTE, we assume that everyone are initially start healthy
# ----------------------------------------------------------------- #


lt_all <- function(.data) {
  
  z <- .data %>%
    group_by(sex, time) %>%
    mutate(lh = 0, lu = 0) %>%
    mutate(lh = c(1, lh[-1])) %>%
    group_nest() %>%
    mutate(data = map(data, ~ .x %>%
                        lets() %>%
                        calculate_lt())) %>%
    unnest(data) %>%
    group_by(sex, time) %>%
    mutate(
      log_mx = log(mx),
      Tx = rev(cumsum(rev(Lx))),
      ex = Tx / lx,
      log_mux = log(mu),
      log_mhx = log(mh),
      hle = rev(cumsum(rev(Lh))) / lx,
      ule = rev(cumsum(rev(Lu))) / lx,
      tst = hle + ule
    ) %>%
    ungroup()
  
  return(z)
}

lt_self  <- extr_self  %>% lt_all()
lt_chron <- extr_chron %>% lt_all()
lt_gali  <- extr_gali  %>% lt_all()



save(lt_self,  file = "Results/lt_self.RData")         
save(lt_chron, file = "Results/lt_chron.RData")         
save(lt_gali,  file = "Results/lt_gali.RData")         


# diagnostic plot
# what really different is the mux across years
# ----------------------------------------------------------------- #
lt_self %>%
  group_by(sex, time) %>% 
  mutate(log_mx = log(mx),
         Tx = rev(cumsum(rev(Lx))),
         ex = Tx / lx,
         log_mux = log(mu),
         log_mhx = log(mh)) %>% 
  ungroup() %>% 
  dplyr::select(sex, time, age, lx, dx, qx, px, Lx, 
                log_mx, Tx, ex, log_mux, log_mhx) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "measure",
               values_to = "val") %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = val, color = time)) + 
  geom_line() + 
  facet_wrap(sex ~ measure, scales = "free_y") + 
  theme_light() + 
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(strip.placement = "outside",
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))

# diagnostic plot of ex hle and uhle
# ----------------------------------------------------------------- #
lt_self %>%
  dplyr::select(sex, time, age, ex, hle, ule) %>% 
  pivot_longer(-c(sex, time, age),
               names_to = "measure",
               values_to = "val") %>%
  mutate(time = as.factor(time)) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = val, color = time)) +
  theme_light() +
  facet_wrap(sex ~ measure)+
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_light() + 
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold")
  )



lt_self %>%
  dplyr::select(sex, time, age, lu, lh) %>% 
  pivot_longer(-c(sex, time, age),
               names_to = "measure",
               values_to = "val") %>%
  mutate(time = as.factor(time)) %>% 
  ggplot() + 
  geom_line(aes(x = age, y = val, color = time)) +
  theme_light() +
  facet_wrap(sex ~ measure)+
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_light() + 
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold")
  )

# ----------------------------------------------------------------- #
# save Ra
Ra_self <- lt_self %>% 
  dplyr::select(sex, time, age, Ra, starts_with("m"), ends_with("D"))

Ra_chron <- lt_chron %>% 
  dplyr::select(sex, time, age, Ra, starts_with("m"), ends_with("D"))

Ra_gali <- lt_gali %>% 
  dplyr::select(sex, time, age, Ra, starts_with("m"), ends_with("D"))

save(Ra_self, file = "Results/Ra_self.RData")
save(Ra_chron, file = "Results/Ra_chron.RData")
save(Ra_gali, file = "Results/Ra_gali.RData")

# ----------------------------------------------------------------- #
# diagnostic plot looks ok
Ra_gali %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line()+
  facet_wrap( ~ sex) +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_light() + 
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))


Ra_chron %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line()+
  facet_wrap( ~ sex) +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_light() + 
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))


Ra_self %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line()+
  facet_wrap( ~ sex) +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_light() + 
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))
