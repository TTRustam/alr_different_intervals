# -----------------------------------------------------------------------------#
# source function
source("clean_code_2/0_packages.R")
source("clean_code_2/04_functions_prevalence.R")


# save(lt_self,  file = "updated_results/lt_self_2.RData")
# save(lt_chron, file = "updated_results/lt_chron_2.RData")
# save(lt_gali,  file = "updated_results/lt_gali_2.RData")
# save(lt_adl,   file = "updated_results/lt_adl_2.RData")
# save(lt_iadl,  file = "updated_results/lt_iadl_2.RData")
# 
# 
# save(Ra_self,  file = "updated_results/Ra_self_2.RData")
# save(Ra_chron, file = "updated_results/Ra_chron_2.RData")
# save(Ra_gali,  file = "updated_results/Ra_gali_2.RData")
# save(Ra_adl,   file = "updated_results/Ra_adl_2.RData")
# save(Ra_iadl,  file = "updated_results/Ra_iadl_2.RData")


# load databases
load("updated_results/self_health.RData")
load("updated_results/lt_self_2.RData")

load("updated_results/chronic.RData")
load("updated_results/lt_chron_2.RData")

load("updated_results/gali.RData")
load("updated_results/lt_gali_2.RData")

load("updated_results/adl.RData")
load("updated_results/lt_adl_2.RData")

load("updated_results/iadl.RData")
load("updated_results/lt_iadl_2.RData")
# -----------------------------------------------------------------------------#
# save prevalence
new_data <- expand_grid(age   = seq(20, 110, 2),
                        time  = unique(self$time),
                        sex   = c("male", "female"))
# -----------------------------------------------------------------------------#
prev_self  <- prev_create(data_lt      = lt_self,  
                          data_initial = self)
prev_chron <- prev_create(data_lt      = lt_chron, 
                          data_initial = chron)
prev_gali  <- prev_create(data_lt      = lt_gali,  
                          data_initial = gali) 
prev_adl  <- prev_create(data_lt      = lt_adl,  
                          data_initial = adl) 
prev_iadl  <- prev_create(data_lt      = lt_iadl,  
                          data_initial = iadl) 

prev_gali %>%
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

prev_chron %>%
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

prev_adl %>%
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

prev_iadl %>%
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


save(prev_self,  file = "updated_results/prev_self_2.RData")
save(prev_chron, file = "updated_results/prev_chron_2.RData")
save(prev_gali,  file = "updated_results/prev_gali_2.RData")
save(prev_adl,  file = "updated_results/prev_adl_2.RData")
save(prev_iadl,  file = "updated_results/prev_iadl_2.RData")
