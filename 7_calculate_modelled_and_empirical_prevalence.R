# ----------------------------------------------------------------- #
# source function
source("functions.R")

# load database
load("Results/self_health.RData")
load("Results/lt_self.RData")

load("Results/chronic.RData")
load("Results/lt_chron.RData")

load("Results/gali.RData")
load("Results/lt_gali.RData")
# predict data

# "Results/extr_self.RData"

# ----------------------------------------------------------------- #

new_data <- expand_grid(age   = unique(self_health$age),
                        time  = unique(self_health$time),
                        sex   = c("male", "female"))



prev_create <- function(data_lt = lt_gali, data_initial = gali) { 
  
# modeled prevalence data
md_prev <- data_lt %>% 
  dplyr::select(sex, time, age, lu, lx) %>%
  mutate(md_prv = lu / lx)

# looks good
# md_prev %>%
#   ggplot(aes(x = age, y = md_prv, color = as.factor(time))) +
#   geom_line() +
#   facet_wrap(~ sex) +
#   theme_bw()

# model prevalence
# ----------------------------------------------------------------- #

prev <- data_initial %>%
  distinct() %>% 
  # new weight
  count(sex, time, age, from) %>%
  group_by(sex, time, age) %>% #from 
  summarise(N = sum(n[from == "U"]), # empirical prevalence
            n = sum(n), .groups = "drop") %>%
  mutate(prev = N / n) %>% 
  group_nest(sex) %>%
  # model the prevalence rate with binomial logit
  mutate(model =  map(data, ~ glm(
    prev ~ time + age,
    weights = n,
    family = binomial(link = "logit"),
    data    = .x
  ))) %>% 
  ungroup() %>%
  # predict
  nest_join(new_data, by = "sex") %>% 
  mutate(predicted_data = map2(.x = model, .y = new_data, ~ predict(.x, .y, type = "response"))) %>%
  mutate(finale = map2(.x = new_data, .y = predicted_data, ~ .x %>%
                         bind_cols(.y) %>% 
                         set_names(c(names(.)[c(1:2)], "case"))))


# model prevalence
# ----------------------------------------------------------------- #
mod_prev <- prev %>%
  dplyr::select(sex, finale) %>% 
  unnest(finale) %>%
  rename(mod_prev = case)

# diagnostic plot
# ----------------------------------------------------------------- #
# mod_prev %>%
#   mutate(time = factor(time)) %>%
#   ggplot(aes(x = age, y = mod_prev, color = time)) +
#   geom_line() +
#   theme_bw() +
#   facet_wrap(~ sex) +
#   theme(strip.background = element_blank(),
#         legend.position = "bottom")

emp_prev <- data_initial %>%
  distinct() %>% 
  # new weight
  count(sex, time, age, from) %>%
  group_by(sex, time, age) %>% #from 
  summarise(N = sum(n[from == "U"]), # empirical prevalence
            n = sum(n)) %>%
  ungroup() %>% 
  mutate(prev = N / n) %>%
  dplyr::select(sex, time, age, emp_prev = prev)

# empirical vs fitted diagnostic
# slightly off in some years, but all in all good fit
# ----------------------------------------------------------------- #
prevalence <- mod_prev %>% 
  full_join(emp_prev) %>%
  full_join(md_prev) %>% 
  dplyr::select(-c(lu, lx))

# prevalence %>%
#   mutate(time = as.factor(time)) %>%
#   mutate(emp_prev = ifelse(emp_prev == 0, NA, emp_prev)) %>% 
#   ggplot() +
#   # this one is lu / lx after alr
#   geom_line(aes(x = age,  y = md_prv, color = sex)) +
#   # this one is fit predict glm after alr
#   geom_line(aes(x = age,  y = mod_prev, color = sex), lty = 2) +
#   # this one is simple ratio
#   geom_point(aes(x = age, y = emp_prev, color = sex)) +
#   facet_wrap(~ time, ncol = 3) +
#   scale_y_continuous(breaks = pretty_breaks())+
#   scale_x_continuous(breaks = pretty_breaks()) +
#   theme_light() + 
#   theme(legend.position = "bottom",
#         strip.text = element_text(color = "black", face = "bold"),
#         axis.title.y = element_blank(),
#         legend.title = element_text(color = "black", face = "bold"),
#         legend.text = element_text(color = "black", face = "bold"),
#         strip.background = element_blank())

return(prevalence)
}
# save prevalence
# ----------------------------------------------------------------- #

prev_self  <- prev_create(data_lt = lt_self,  data_initial = self_health) %>%
  filter(time > 2011)
prev_chron <- prev_create(data_lt = lt_chron, data_initial = chronic) %>%
  filter(time > 2011)
prev_gali  <- prev_create(data_lt = lt_gali,  data_initial = gali) 

prev_self %>%
  mutate(time = as.factor(time)) %>%
  # mutate(emp_prev = ifelse(emp_prev == 0, NA, emp_prev)) %>%
  ggplot() +
  # this one is lu / lx after alr
  geom_line(aes(x = age,  y = md_prv, color = sex)) +
  # this one is fit predict glm after alr
  geom_line(aes(x = age,  y = mod_prev, color = sex), lty = 2) +
  # this one is simple ratio
  geom_point(aes(x = age, y = emp_prev, color = sex)) +
  facet_wrap(~ time, ncol = 3) +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks()) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"),
        strip.background = element_blank())


save(prev_self,  file = "Results/prev_self.RData")
save(prev_chron, file = "Results/prev_chron.RData")
save(prev_gali,  file = "Results/prev_gali.RData")

# RMSE. looks good, if we remove empirical outliers == 1
# 2015 gives the biggest error, but it is just 0.1
# mod_prev %>% 
#   full_join(emp_prev) %>%
#   mutate(time = as.factor(time)) %>% 
#   group_by(sex, time) %>% 
#   replace(is.na(.), 0) %>% 
#   mutate(emp_prev = ifelse(emp_prev == 0.3, 0, emp_prev)) %>% 
#   summarise(rmse = sqrt(sum((mod_prev - emp_prev) ^ 2) / n()), .groups = "drop") %>% 
#   ggplot(aes(x = time, y = rmse, group = sex, color = sex)) + 
#   expand_limits(y = 0) +
#   geom_line() +
#   geom_point()+
#   theme_bw() + 
#   theme(legend.position = "bottom")