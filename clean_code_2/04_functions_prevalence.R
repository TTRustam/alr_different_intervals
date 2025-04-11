# -----------------------------------------------------------------------------#
prev_create <- function(data_lt      = lt_self, 
                        data_initial = self) { 
  
  # modeled prevalence data
  md_prev <- data_lt %>% 
    mutate(lx = lu + lh) %>% 
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
  # empirical prevalence
  prev <- data_initial %>%
    # new weight
    count(sex, time, age, from) %>%
    # filter(age < 91) %>% 
    group_by(sex, time, age) %>% #from
    summarise(N = sum(n[from == "U"]),
              # empirical prevalence
              n = sum(n),
              .groups = "drop") %>%
    mutate(prev = N / n) %>%
    group_nest(sex) %>%
    # model the prevalence rate with binomial logit
    # smooth over age, basic value = 4 p-spline
    mutate(model =  map(data, ~ gam(
      prev ~ age + time,
      weights = n,
      family  = binomial(link = "logit"),
      data    = .x
    ))) %>%
    ungroup() %>%
    # predict
    nest_join(new_data, by = "sex") %>%
    mutate(predicted_data = map2(
      .x = model,
      .y = new_data,
      ~ predict(.x, .y, type = "response")
    )) %>%
    mutate(finale = map2(.x = new_data, .y = predicted_data, ~ .x %>%
                           mutate(case := .y)))
  
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
  #   # mutate(emp_prev = ifelse(emp_prev == 0, NA, emp_prev)) %>%
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
  #         strip.text = element_text(color   = "black", face = "bold"),
  #         axis.title.y = element_blank(),
  #         legend.title = element_text(color = "black", face = "bold"),
  #         legend.text = element_text(color  = "black", face = "bold"),
  #         strip.background = element_blank())
  
  return(prevalence)
}
# -----------------------------------------------------------------------------#