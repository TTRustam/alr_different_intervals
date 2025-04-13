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
  
  
  
  
 
  
  
    # read_table("Data_HMD/fltper_1x1.txt", skip = 1) %>% 
    #   filter(Year %in% c(2011, 2012)) %>% 
    #   dplyr::select(Year, Age, dx, Lx) %>%
    #   mutate(Age = parse_number(Age)) %>%
    #   pivot_wider(names_from = Year,
    #               values_from = c(dx, Lx)) %>% 
    #   mutate(dx_2012 = lead(dx_2012)) %>% 
    #   mutate(dx = (dx_2011  + dx_2012),
    #          Lx = (Lx_2011 + Lx_2012) / 2) %>% 
    #   mutate(z = dx / Lx,
    #          mx_hmd = z / (2 - z)) %>%
    #     #        Age = parse_number(Age)) %>%
    #     ggplot(aes(x = Age, y = mx_hmd)) +
    #     geom_line() +
    #     scale_y_log10()
    
  hmd_f <- read_table("Data_HMD/fltper_1x1.txt", skip = 1) %>%
    mutate(
      Age = parse_number(Age),
      age = Age - Age %% 2
    ) %>%
    group_by(Year, age) %>% 
    summarize(dx = sum(dx),
              lx = lx[1], .groups = "drop") %>% 
    mutate(qx = dx / lx,
           mx_hmd = qx / (2 - qx)) %>% 
    dplyr::rename(time = Year) %>% 
    mutate(sex = "female") %>% 
    filter(time %in% unique(prev_test$time),
           age %in% unique(prev_test$age))
  
    
  # hmd_f %>%
  #   mutate(time = as.factor(time)) %>%
  #   ggplot(aes(x = age, y = mx_hmd, color = time)) +
  #   geom_line() +
  #   scale_y_log10()

    
  
  hmd_m <- read_table("Data_HMD/mltper_1x1.txt", skip = 1) %>%
    # dplyr::select(Year, Age, qx) %>% 
    mutate(
      Age = parse_number(Age),
      age = Age - Age %% 2
    ) %>%
    group_by(Year, age) %>% 
    summarize(dx = sum(dx),
              lx = lx[1], .groups = "drop") %>% 
    mutate(qx     = dx / lx,
           mx_hmd = qx / (2 - qx)) %>% 
    dplyr::rename(time = Year) %>% 
    mutate(sex = "male") %>% 
    filter(time %in% unique(prev_test$time),
           age %in% unique(prev_test$age))
    # group_by(Year, age) %>%
    # summarise(qx = 1 - prod(1 - qx, na.rm = TRUE),
              # mx_hmd = qx / (2 - qx), .groups = "drop") %>%
    # dplyr::select(time = Year, age, mx_hmd) %>%
    # mutate(sex = "male") %>% 
    # filter(time %in% unique(prev_test$time),
    #        age %in% unique(prev_test$age)) 
  
  
  # hmd_m %>%
  #   mutate(time = as.factor(time)) %>%
  #   ggplot(aes(x = age, y = mx_hmd, color = time)) +
  #   geom_line() +
  #   scale_y_log10()
  # 
  # overall hmd
  hmd <- hmd_f %>%
    full_join(hmd_m) %>% 
    dplyr::select(-c(dx, lx, qx))
  
  # hmd %>%
  #   mutate(time = as.factor(time)) %>%
  #   ggplot(aes(x = age, y = mx_hmd, color = time, lty = sex)) +
  #   geom_line() +
  #   scale_y_log10()
  # recalculate the mortality rates using Tim PAA abstract formula 5 and 4
  # ----------------------------------------------------------------- #
  new_mx <- Ra %>%
    full_join(prev_test) %>%
    full_join(hmd) %>%
    mutate(mh_new = mx_hmd / (1 - mod_prev + mod_prev * Ra), # formula 5
           mu_new = mh_new * Ra) %>% # formula 4
    dplyr::select(-c(`HD`, `UD`)) %>%
    # calculate mortality from probabilities
    # qx from mx q(x) = 1 - exp(-mx)
    mutate(`HD`  =  (2 * mh_new) / (1 + mh_new),
           `UD`  =  (2 * mu_new) / (1 + mu_new)
             # 1 - exp(-mh_new),
           # `UD`  = 1 - exp(-mu_new)
           ) %>%
    dplyr::select(sex, time, age, `HD`, `UD`)
  
  
  # diagnostic plot
  # Ra %>%
  #   full_join(prev_test) %>%
  #   full_join(hmd) %>%
  #   mutate(mh_new = mx_hmd / (1 - mod_prev + mod_prev * Ra), # formula 5
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
  #   filter(age %in% c(50:100)) %>%
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