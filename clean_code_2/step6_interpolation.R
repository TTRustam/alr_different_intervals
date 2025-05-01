# -----------------------------------------------------------------------------#
# packages
source("clean_code_2/0_packages.R")
# functions
source("clean_code_2/06_functions_interpolate.R")
# data
load("updated_results/final_full_transitions_2.RData")
# -----------------------------------------------------------------------------#
# adjusted vs unadjusted 2 year prob.
# confirm last age closeup
final %>%
  filter(time == 2013, health_var == "self_report") %>%
  dplyr::select(-c(1, 4)) %>%
  mutate(adjusted = factor(adjusted, levels = c("yes", "no"))) %>% 
  ggplot(aes(x = age, y = prob, color = to, lty = adjusted)) + 
  geom_line(linewidth = 1) + 
  facet_grid(sex ~ from, switch = "y") + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        strip.placement = "outside")

# check chronic from input data
final %>% 
  filter(health_var == "chronic") %>% 
  filter(adjusted == "no") %>% 
  dplyr::select(-c(adjusted)) %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = prob, color = to, 
             linetype = time)) + 
  geom_line(linewidth = 1)+
  scale_linetype_manual(
    values = c(
      "2011" = "solid",
      "2013" = "dashed",
      "2015"  = "dotted"
    )) +
  theme_minimal() + 
  facet_grid(sex ~ from, switch = "y") + 
  theme(legend.position = "bottom",
        strip.placement = "outside") 

# -----------------------------------------------------------------------------#
# interpolation
# both analityc and hackish
# Currently I do not do chronic
# with chronic problem is negative eigenvalue in last age in year 2015
one_year <- final %>% 
  filter(health_var != "chronic") %>% 
  filter(adjusted == "no") %>% 
  dplyr::select(-c(adjusted)) %>% 
  unite("trns", c(from, to), sep = "") %>%
  pivot_wider(names_from  = trns,
              values_from = prob) %>% 
  group_nest(health_var, sex, time) %>%
  mutate(final = map(data, ~ .x %>% 
                       interpolate_prob())) %>% 
  mutate(final_tim = map(data, ~ .x %>%
                           do_grad())) %>% 
  mutate(data = map(data, ~ .x %>% 
                      pivot_longer(-age,
                                   names_to  = "trns",
                                   values_to = "prob") %>% 
                      mutate(from = str_sub(trns, 1, 1),
                             to   = str_sub(trns, 2)) %>% 
                      dplyr::select(-trns))) %>% 
  mutate(all_data = pmap(list(data, final, final_tim), 
                         function(d, f, g) {
    bind_rows(
      mutate(d, source = "original"),
      mutate(f, source = "analytic"),
      mutate(g, source = "hackish")
    )
  })) %>%
  select(-c(data, final, final_tim)) %>%
  unnest(all_data)

# -----------------------------------------------------------------------------#
# visualization of results
one_year %>%
  mutate(age = ifelse(source == "hackish", age - 1, age)) %>%
  filter(time == 2013, health_var == "iadl") %>%
  filter(between(age, 20, 110)) %>%
  # iadl in age 110 in year 2013 gives a small negative for one transition
  # just take abs and normalize
  mutate(prob = abs(prob)) %>% 
  group_by(health_var, sex, time, age, source, from) %>% 
  mutate(s = sum(prob)) %>% 
  mutate(prob = ifelse(s > 1, prob / sum(prob), prob)) %>%
  mutate(s = sum(prob)) %>% 
  ggplot(aes(x = age, y = prob, color = to, linetype = source)) + 
  geom_line(linewidth = 1)+
  scale_linetype_manual(
    values = c(
      "original" = "solid",
      "analytic" = "dashed",
      "hackish"  = "dotted"
    )) +
  theme_minimal() + 
  facet_grid(sex ~ from, switch = "y") + 
  theme(legend.position = "bottom",
        strip.placement = "outside") 

# -----------------------------------------------------------------------------#

# adjust data for the iadl like in the figure
one_year <- one_year %>%
  mutate(age = ifelse(source == "hackish", age - 1, age)) %>%
  filter(between(age, 20, 110)) %>%
  # iadl in age 110 in year 2013 gives a small negative for one transition
  # just take abs
  mutate(prob = abs(prob)) %>% 
  group_by(health_var, sex, time, age, source, from) %>% 
  mutate(s = sum(prob)) %>% 
  mutate(prob = ifelse(s > 1, prob / sum(prob), prob)) %>%
  dplyr::select(-s) %>% 
  ungroup()

# check. all good
one_year %>% 
  group_by(health_var, sex, time, age, source, from) %>% 
  mutate(s = sum(prob)) %>% 
  filter(s > 1)

save(one_year,  file = "updated_results/graduated_prob.RData")
