# ----------------------------------------------------------------- #
# source function
source("functions.R")

# load database
load("Results/Ra_self.RData")
load("Results/prev_self.RData")
load("Results/extr_self.RData")

load("Results/Ra_chron.RData")
load("Results/prev_chron.RData")
load("Results/extr_chron.RData")

load("Results/Ra_gali.RData")
load("Results/prev_gali.RData")
load("Results/extr_gali.RData")


adjust <- function(Ra = Ra_gali, 
                   prev = prev_gali, 
                   extrap_dat = extr_gali) { 

prev_test <- prev %>%
  dplyr::select(-c(md_prv, emp_prev)) %>% 
  rename(case = mod_prev)

# read and filter hmd female data for males and females for Spain
# ----------------------------------------------------------------- #
hmd_f <- read_table("Data/fltper_1x1.txt", skip = 1) %>%
  mutate(mx_hmd = mx) %>% 
  dplyr::select(time = Year, age = Age, mx_hmd) %>%
  mutate(age = parse_number(age)) %>%
  mutate(sex = "female") %>% 
  filter(time %in% unique(prev_test$time),
         age %in% unique(prev_test$age)) 

hmd_m <- read_table("Data/mltper_1x1.txt", skip = 1) %>%
  mutate(mx_hmd = mx) %>% 
  dplyr::select(time = Year, age = Age, mx_hmd) %>%
  mutate(age = parse_number(age)) %>%
  mutate(sex = "male") %>% 
  filter(time %in% unique(prev_test$time),
         age %in% unique(prev_test$age)) 

# overall hmd
hmd <- hmd_f %>%
  full_join(hmd_m)

# recalculate the mortality rates using Tim PAA abstract formula 5 and 4
# ----------------------------------------------------------------- #
new_mx <- Ra %>%
  full_join(prev_test) %>%
  full_join(hmd) %>%
  mutate(mh_new = mx_hmd / (1 - case + case * Ra), # formula 5
         mu_new = mh_new * Ra) %>% # formula 4
  dplyr::select(-c(`HD`, `UD`)) %>%
  # calculate mortality from probabilities
  # qx from mx q(x) = 1 - exp(-mx)
  mutate(`HD`  = 1 - exp(-mh_new),
         `UD` = 1 - exp(-mu_new)) %>%
  dplyr::select(sex, time, age, `HD`, `UD`)

# diagnostic plot
# new mortality is always lower than old mortality
# unhealty mortality is very high always
# ----------------------------------------------------------------- #
# Ra %>%
#   full_join(prev_test) %>%
#   full_join(hmd) %>%
#   mutate(mh_new = mx_hmd / (1 - case + case * Ra), # formula 5
#          mu_new = mh_new * Ra) %>% # formula 4
#   dplyr::select(sex, time, age, mh_old = mh, mu_old = mu, mh_new, mu_new) %>%
#   pivot_longer(-c(sex, time, age),
#                names_to  = "variable",
#                values_to = "val") %>%
#   separate(variable, c("indicator", "old_new")) %>%
#   mutate(time = as.factor(time)) %>%
#   filter(sex == "male") %>%
#   ggplot(aes(x = age, y = val,
#              group = interaction(old_new, indicator),
#              color = indicator,
#              linetype = old_new)) +
#   geom_line() +
#   scale_y_log10() +
#   facet_wrap( ~ time, strip.position = "left", ncol = 3) +
#   theme_bw() +
#   theme(strip.background = element_blank(),
#         axis.title.y = element_blank(),
#         legend.position = "bottom",
#         strip.placement = "outside")

# recalculate the transition probabilities with new mx values
# ----------------------------------------------------------------- #
old_trns <- extrap_dat %>% 
  dplyr::select(-c(`HD`, `UD`))

new_trans <- old_trns %>% 
  full_join(new_mx) %>% 
  pivot_longer(-c(sex, age, time),
               names_to  = "var",
               values_to = "val") %>%
  mutate(from = str_sub(var, start = 1, end = 1)) %>%
  mutate(to   = str_sub(var, 2)) %>%
  dplyr::select(-var)%>%
  group_by(sex, time, age, from) %>% 
  mutate(val = recalculate(val)) %>% 
  mutate(type = "Adjusted") %>% 
  ungroup()

# old transition for plot
old_plot <- extrap_dat %>% 
  pivot_longer(-c(sex, age, time),
               names_to  = "var",
               values_to = "val") %>% 
  mutate(from = str_sub(var, start = 1, end = 1)) %>%
  mutate(to   = str_sub(var, 2)) %>%
  mutate(type = "Raw") %>% 
  ungroup()

full_trns <- new_trans %>% 
  full_join(old_plot)

# diagnostic plot of old and new transitions
# transitions to death are higher now
# we have this jump in the last year. 
# ----------------------------------------------------------------- #
# full_trns %>%
#   filter(time == 2013) %>%
#   ggplot() +
#   geom_line(aes(x = age, y = val, color = to, lty = type), linewidth = 1) +
#   facet_wrap(from ~ sex) +
#   theme_light() +
#   scale_y_continuous(breaks = pretty_breaks()) +
#   scale_x_continuous(breaks = pretty_breaks()) +
#   theme(legend.position = "bottom",
#         strip.text = element_text(color = "black", face = "bold"),
#         axis.title.y = element_blank(),
#         strip.background = element_blank(),
#         legend.title = element_text(color = "black", face = "bold"),
#         legend.text = element_text(color = "black", face = "bold"))

return(full_trns)

}


final_self  <- adjust(Ra         = Ra_self,
                      prev       = prev_self,
                      extrap_dat = extr_self) 

final_chron <- adjust(Ra         = Ra_chron,
                      prev       = prev_chron,
                      extrap_dat = extr_chron) 

final_gali  <- adjust(Ra         = Ra_gali,
                      prev       = prev_gali,
                      extrap_dat = extr_gali) 


# save
save(final_self,  file = "Results/final_self.RData")
save(final_chron, file = "Results/final_chron.RData")
save(final_gali,  file = "Results/final_gali.RData")


final_self <- final_self %>% 
  filter(type == "Adjusted") %>% 
  dplyr::select(-type) %>% 
  rename(p    = val,
         year = time) %>% 
  mutate(type = "self_rated")


final_chron <-  final_chron %>% 
  filter(type == "Adjusted") %>% 
  dplyr::select(-type) %>% 
  rename(p    = val,
         year = time)%>% 
  mutate(type = "chronic")


final_gali <- final_gali %>% 
  filter(type == "Adjusted") %>% 
  dplyr::select(-type) %>% 
  rename(p    = val,
         year = time) %>% 
  mutate(type = "gali")

final_share <- final_self %>% 
  full_join(final_chron) %>% 
  full_join(final_gali) %>% 
  filter(age > 49) %>% 
  dplyr::select(-var)

save(final_share, file = "Results/final_share.RData")





