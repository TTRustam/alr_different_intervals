# -----------------------------------------------------------------------------#
# source function
source("clean_code_2/0_packages.R")
# load database
load("updated_results/graduated_prob.RData")


# -----------------------------------------------------------------------------#
# calculate lifetable. keep ages 20-110

lets <- function(.data) {
  
  for(i in 1:length(.data$lu[-1])) {
    
    .data$lh[i + 1] <-
      .data$lh[i] * .data$`HH`[i] + .data$lu[i] * .data$`UH`[i]
    
    .data$lu[i + 1] <-
      .data$lu[i] * .data$`UU`[i] + .data$lh[i] * .data$`HU`[i]
    
  }
  
  return(.data)
  
}



calculate_Ra <- function(.data) { 
  
  .data %>% 
    group_by(health_var, sex, time) %>% 
    # mutate(mh = HD / (2 - HD),
    #        mu = UD / (2 - UD)) %>% 
    mutate(Ra = UD / HD) # here changed for probability ratio
}

# prepare database
lt_one_year <- one_year %>% 
  filter(source == "analytic") %>% 
  dplyr::select(-source) %>% 
  unite("trns", c(from, to), sep = "") %>%
  pivot_wider(names_from  = trns,
              values_from = prob) %>% 
  calculate_Ra() %>% 
  group_by(health_var, sex, time) %>%
  mutate(lh = 0, lu = 0) %>%
  mutate(lh = c(1, lh[-1])) %>%
  group_nest() %>%
  mutate(data = map(data, ~ .x %>%
                      lets())) %>% 
  unnest(data)

# save Ra
Ra_one_year <- lt_one_year %>% 
  dplyr::select(health_var, sex, time, age, Ra, ends_with("D"))

# tests mortality rates
lt_one_year %>% 
  filter(health_var == "self_report") %>% 
  dplyr::select(sex, time, age, UD, HD) %>% 
  mutate(time = as.factor(time)) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "qx",
               values_to = "val") %>% 
  ggplot(aes(x = age, y = val, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(qx~  sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")


lt_one_year %>% 
  filter(health_var == "self_report")  %>% 
  dplyr::select(sex, time, age, Ra) %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line() + 
  # scale_y_log10()+
  facet_wrap(~ sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------#

save(lt_one_year,  file = "updated_results/lt_one_year.RData")
save(Ra_one_year,  file = "updated_results/Ra_one_year.RData")


# -----------------------------------------------------------------------------#
# diagnostic plot lh lu
lt_one_year %>% 
  filter(health_var == "self_report") %>% 
  dplyr::select(sex, time, age, lu, lh) %>% 
  mutate(time = as.factor(time)) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "mx",
               values_to = "val") %>% 
  ggplot(aes(x = age, y = val, color = time)) + 
  geom_line() + 
  facet_wrap(mx~  sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

(lt_one_year$lh + lt_one_year$lu)[1]
