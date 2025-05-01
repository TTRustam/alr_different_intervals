# -----------------------------------------------------------------------------#
# source function
source("clean_code_2/0_packages.R")
source("clean_code_2/04_functions_prevalence.R")

load("updated_results/lt_one_year.RData")

load("updated_results/self_health.RData")
load("updated_results/gali.RData")
load("updated_results/adl.RData")
load("updated_results/iadl.RData")

# load("updated_results/Ra_one_year.RData")
# load("updated_results/graduated_prob.RData")


# -----------------------------------------------------------------------------#
# save prevalence
new_data <- expand_grid(age   = seq(20, 110, 1),
                        time  = unique(self$time),
                        sex   = c("male", "female"))
# -----------------------------------------------------------------------------#
prev_self  <- prev_create(data_lt      = filter(lt_one_year, 
                                                health_var == "self_report"),  
                          data_initial = self)

prev_gali  <- prev_create(data_lt      = filter(lt_one_year, 
                                                health_var == "gali"),  
                          data_initial = gali) 
prev_adl  <- prev_create(data_lt      = filter(lt_one_year, 
                                               health_var == "adl"),  
                          data_initial = adl) 
prev_iadl  <- prev_create(data_lt      = filter(lt_one_year, 
                                                health_var == "iadl"),  
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


save(prev_self,  file = "updated_results/prev_self_1.RData")
save(prev_gali,  file = "updated_results/prev_gali_1.RData")
save(prev_adl,   file = "updated_results/prev_adl_1.RData")
save(prev_iadl,  file = "updated_results/prev_iadl_1.RData")