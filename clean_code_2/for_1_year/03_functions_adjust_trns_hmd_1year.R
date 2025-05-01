recalculate <- function(x) {
  
  x[1:2] <- x[1:2] / sum(x[1:2]) * (1 - x[3])
  
  return(x)
  
}
# -----------------------------------------------------------------------------#

adjust <- function(Ra         = Ra_self, 
                   prev       = prev_self, 
                   extrap_dat = extr_self) { 
  
  # choose prevalence from logistic regression
  prev_test <- prev %>%
    dplyr::select(-c(md_prv, emp_prev))
  
  # read and filter hmd female data for males and females for Spain
  # ----------------------------------------------------------------- #
  
  hmd <- new_lt %>% 
    dplyr::select(sex, time, age, qx_hmd = qx) %>% 
    filter(age > 19)
  
  # hmd %>%
  #   mutate(time = as.factor(time)) %>%
  #   ggplot(aes(x = age, y = qx_hmd, color = time, lty = sex)) +
  #   geom_line() +
  #   scale_y_log10()
  # recalculate the mortality rates using Tim PAA abstract formula 5 and 4
  # ----------------------------------------------------------------- #
  
  # check this one carefully
  new_mx <- Ra %>%
    full_join(prev_test) %>%
    full_join(hmd) %>%
    mutate(HD = qx_hmd / (1 - mod_prev + mod_prev * Ra), # formula 5
           UD = HD * Ra) %>% # formula 4
    dplyr::select(sex, time, age, HD, UD) %>%
    # here UD in the last age some times > 1
    # in this case we close with average between 1 and last value
    group_by(sex, time) %>% 
    mutate(HD = ifelse(HD > 1, dplyr::lag(HD), HD)) %>% 
    mutate(UD = ifelse(UD > 1, dplyr::lag(UD), UD)) %>% 
    ungroup()
  # diagnostic plot
  # Ra %>%
  #   full_join(prev_test) %>%
  #   full_join(hmd) %>%
  #   mutate(mh_new = qx_hmd / (1 - mod_prev + mod_prev * Ra), # formula 5
  #          mu_new = mh_new * Ra) %>% # formula 4
  #   dplyr::select(sex, time, age, mh_old = mh, mu_old = mu, mh_new, mu_new) %>%
  #   pivot_longer(-c(sex, time, age),
  #                names_to  = "variable",
  #                values_to = "val") %>%
  #   separate(variable, c("indicator", "old_new")) %>%
  #   mutate(time = as.factor(time)) %>%
  #   ggplot(aes(x = age, y = val,
  #              group = interaction(old_new, indicator),
  #              color = indicator,
  #              linetype = old_new)) +
  #   geom_line() +
  #   scale_y_log10() +
  #   facet_wrap(sex ~ time, strip.position = "left", ncol = 3) +
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
    dplyr::select(-health_var) %>% 
    full_join(new_mx) %>% 
    pivot_longer(-c(sex, age, time),
                 names_to  = "var",
                 values_to = "val") %>%
    mutate(from = str_sub(var, start = 1, end = 1)) %>%
    mutate(to   = str_sub(var, 2)) %>%
    dplyr::select(-var) %>%
    group_by(sex, time, age, from) %>% 
    mutate(val = recalculate(val)) %>% 
    mutate(type = "Adjusted") %>% 
    ungroup()
  
  # old transition for plot
  old_plot <- extrap_dat %>% 
    dplyr::select(-health_var) %>% 
    pivot_longer(-c(sex, age, time),
                 names_to  = "var",
                 values_to = "val") %>% 
    mutate(from = str_sub(var, start = 1, end = 1)) %>%
    mutate(to   = str_sub(var, 2)) %>%
    mutate(type = "Raw") %>% 
    ungroup() %>% 
    dplyr::select(-var)  
  
  full_trns <- new_trans %>% 
    full_join(old_plot) 
  
  # diagnostic plot of old and new transitions
  # ----------------------------------------------------------------- #
  # full_trns %>%
  #   filter(time == 2013) %>%
  #   # filter(age %in% c(50:100)) %>%
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

