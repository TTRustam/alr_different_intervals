# -----------------------------------------------------------------------------#
source("clean_code_2/0_packages.R")

# load HMD data
Dx <- read_table("Data_HMD/Dx_Ex/Deaths_1x1.txt", skip = 1) %>%
  dplyr::select(-Total) %>%
  mutate(age = parse_number(Age)) %>%
  filter(Year %in% c(2011, 2013, 2015)) %>% 
  dplyr::select(time = Year, age, Male, Female) %>% 
  pivot_longer(-c(time, age),
               names_to  = "sex",
               values_to = "Dx")

# Load Exposures
Ex <- read_table("Data_HMD/Dx_Ex/Exposures_1x1.txt", skip = 1) %>%
  dplyr::select(-Total) %>%
  mutate(age = parse_number(Age)) %>%
  filter(Year %in% c(2011, 2013, 2015)) %>% 
  dplyr::select(time = Year, age, Male, Female) %>% 
  pivot_longer(-c(time, age),
               names_to  = "sex",
               values_to = "Ex")

# full mx
hmd_mx <- Dx %>% 
  full_join(Ex) %>% 
  mutate(sex = tolower(sex),
         mu  = Dx / Ex)

# hmd lifetable
hmd_m <- read_table("Data_HMD/mltper_1x1.txt", skip = 1) %>%
  filter(Year %in% c(2011, 2013, 2015)) %>%
  mutate(age = parse_number(Age)) %>%
  dplyr::select(time = Year, age, mu_hmd = mx) %>% 
  mutate(sex = "male")

hmd_f <- read_table("Data_HMD/fltper_1x1.txt", skip = 1) %>%
  filter(Year %in% c(2011, 2013, 2015)) %>%
  mutate(age = parse_number(Age)) %>%
  dplyr::select(time = Year, age, mu_hmd = mx)%>% 
  mutate(sex = "female")

# full lt
hmd_lt <- hmd_f %>% 
  full_join(hmd_m)

# Merge 
hmd <- hmd_mx %>% 
  full_join(hmd_lt)
# -----------------------------------------------------------------------------#
# smoothing
# one of the Ex in age 110 is zero, replaced by the precious one
hmd <- hmd %>%
  group_by(sex, time) %>%
  mutate(Ex = ifelse(Ex == 0, lag(Ex), Ex)) %>%
  group_nest() %>%
  mutate(result = map2(data, sex, ~ {
    if (.y == "female") {
      gam(
        Dx ~ s(age, bs = "ps", m = 1, k = 11),
        offset = log(Ex),
        family = poisson(link = "log"),
        data = .
      )
    } else {
      gam(
        Dx ~ s(age, bs = "ps", m = 2, k = 11),
        offset = log(Ex),
        family = poisson(link = "log"),
        data = .
      )
    }
  }),
  data = map2(data, result, ~ .x %>%
                mutate(mu_hat = predict(
                  .y, newdata = .x, type = "response"
                )))) %>% 
  dplyr::select(-result) %>% 
  unnest(data) %>% 
  dplyr::select(-c(Dx, Ex))


hmd %>%
  ggplot(aes(x = age)) +
  geom_line(aes(
    y = mu,
    color = "Observed",
    linetype = "Observed"
  ), linewidth = 1) +    # Observed
  geom_line(aes(
    y = mu_hmd,
    color = "HMD",
    linetype = "HMD"
  ), linewidth = 1) + # HMD
  geom_line(aes(
    y = mu_hat,
    color = "Smooth",
    linetype = "Smooth"
  ), linewidth = 1) +  # Smooth
  scale_y_log10() +
  labs(y = "Mortality Rate (Log Scale)", x = "Age") +
  scale_color_manual(values = c(
    "Observed"  = "black",
    "HMD"       = "red",
    "Smooth" = "green"
  )) +
  scale_linetype_manual(values = c(
    "Observed" = "solid",
    "HMD"   = "dashed",
    "Smooth" = "dotted"
  )) +
  facet_wrap(time ~ sex) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  guides(color    = guide_legend(title = "Model"),
         linetype = guide_legend(title = "Model"))


new_lt <- hmd %>%
  group_by(sex, time) %>% 
  mutate(qx = (2 * mu_hat) / (2 + mu_hat)) %>% 
  ungroup()
  

new_lt %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = qx, color = time)) + 
  geom_line() + 
  facet_wrap(~ sex) + 
  scale_y_log10() + 
  theme_minimal()+
  theme(legend.position = "bottom")

save(new_lt,  file = "updated_results/smooth_hmd_qx.RData")
