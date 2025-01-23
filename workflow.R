library(tidyverse)
library(expm)
library(compositions)
source("functions.R")
load("Data/transition_probs_model_raw.RData")

# transition probabilities from the multinomial model
trns <- tst %>%
  filter(sex == "male") %>% # NOW for males | change to females if needed
  unnest(qxdata) %>% 
  dplyr::select(-sex) %>% 
  filter(time == 2010) %>% 
  dplyr::select(-time) %>%
  set_names(c("age", 
              "HH", "HU", "HD",
              "UH", "UU", "UD"))

# check. ok
trns %>% 
  dplyr::select(-age) %>% 
  dplyr::select(starts_with("U")) %>% 
  rowSums()

# 1
# HH <- pi2u(
#   pull(trns, "HH"),
#   from = "H",
#   to   = "H",
#   start_age = 17,
#   interval  = 1)
# HU <- pi2u(
#   pull(trns, "HU"),
#   from = "H",
#   to   = "U",
#   start_age = 17,
#   interval  = 1)
# UH <- pi2u(
#   pull(trns, "UH"),
#   from = "U",
#   to   = "H",
#   start_age = 17,
#   interval  = 1
# )
# UU <- pi2u(
#   pull(trns, "UU"),
#   from = "U",
#   to   = "U",
#   start_age = 17,
#   interval  = 1
# )
# U <- u2U_closed(HH, HU, UH, UU)


# initial workflow steps
# In this step one, We slightly move the trans. prob. matrix to not be singular
U        <- Ptibble2U_closed(trns,
                             interval  = 1, 
                             start_age = 17)
Q        <- U2Q(U, interval_current = 1)
Rtibble  <- Q2Rtibble(Q, interval_current = 1)
Rtibble1 <- graduate_Rtibble(Rtibble, interval_current = 1, interval_desired = 0.25)
# Q1       <- Rtibble2Q(Rtibble1, interval = 0.25, start_age = 17)
# U1       <- Q2U(t(Q1), interval_desired = 0.25)
# Ptibble1 <- U2Ptibble(U1, interval_current = 0.25)

# dimnames(Q1)
# Ptibble1$age %>% unique()



# Here we have the results for the chosen interpolation length
# function does the same as lines 62:64 but changes the intervals
year1    <- var_interval(interval_desired = 1)
year2    <- var_interval(interval_desired = 2)
year0.5  <- var_interval(interval_desired = 0.5)
year0.25 <- var_interval(interval_desired = 0.25)


# year0.25 %>%
#   dplyr::select(-age) %>% 
#   rowSums()

# check
year0.5 %>%
  pivot_longer(-age,names_to = "transition",values_to = "p") |> 
  ggplot(aes(x = age,y = p, color = transition)) + 
  geom_line()

# a <- year2 %>% 
#   make_plots()
# 
# b <- year1 %>% 
#   make_plots()
# 
# d <- year0.5 %>% 
#   make_plots()
# 
# g <- year0.25 %>% 
#   make_plots()
# 
# (a + b  + d + g)


# create a figure
year2 <- year2 %>% 
  mutate(type = "two")
year1 <- year1 %>% 
  mutate(type = "one")
year0.5 <- year0.5 %>% 
  mutate(type = "half")
year0.25 <- year0.25 %>% 
  mutate(type = "quarter")

# figure for healthy 
year2 %>% 
  full_join(year1) %>% 
  full_join(year0.5) %>% 
  full_join(year0.25) %>% 
  make_plots_tim_H()

# same for unhealthy
year2 %>% 
  full_join(year1) %>% 
  full_join(year0.5) %>% 
  full_join(year0.25) %>% 
  make_plots_tim_U()


# ratios for UD
age <- year2$age
a <- year2 %>% 
dplyr::select(age, starts_with("U")) %>%
  dplyr::select("UU", "UD", "UH") %>%
  as.matrix() %>%
  alr() %>%
  as_tibble() %>% 
  dplyr::select(UD)%>% 
  mutate(age = age) %>% 
  mutate(type = "2")

age <- year1$age
b <- year1 %>% 
  dplyr::select(age, starts_with("U")) %>%
  dplyr::select("UU", "UD", "UH") %>%
  as.matrix() %>%
  alr() %>%
  as_tibble() %>% 
  dplyr::select(UD)%>% 
  mutate(age = age) %>% 
  mutate(type = "1")

age <- year0.5$age
d <- year0.5 %>% 
  dplyr::select(age, starts_with("U")) %>%
  dplyr::select("UU", "UD", "UH") %>%
  as.matrix() %>%
  alr() %>%
  as_tibble() %>% 
  dplyr::select(UD)%>% 
  mutate(age = age) %>% 
  mutate(type = "0.5")

age <- year0.25$age
g <- year0.25 %>% 
  dplyr::select(age, starts_with("U")) %>%
  dplyr::select("UU", "UD", "UH") %>%
  as.matrix() %>%
  alr() %>%
  as_tibble() %>% 
  dplyr::select(UD) %>% 
  mutate(age = age) %>% 
  mutate(type = "0.25")

# ratios of alr
dt <- a %>% 
  full_join(b) %>%
  full_join(d) %>% 
  full_join(g) %>% 
  pivot_wider(names_from = type,
              values_from = UD) %>% 
  mutate(
    rate_2_1 = `2` / `1`,
    rate_1_0.5 = `1` / `0.5`,
    rate_0.5_0.25 = `0.5` / `0.25`
  ) %>% 
  dplyr::select(age, starts_with("rate_")) %>% 
  pivot_longer(-age,
               names_to  = "var",
               values_to = "val") %>% 
  mutate(var = as.factor(var))

# plot ratios for alr UD
dt %>% 
  ggplot(aes(x = age, y = val, color = `var`)) + 
  geom_point() + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_color_manual(labels = c("half to quarter", 
                                "one to half",
                                "two to one"), values = c("blue", "red", "black"))

# some plots for for unhealthy
a <- year2 %>% 
  make_plotsU()

b <- year1 %>% 
  make_plotsU()

d <- year0.5 %>% 
  make_plotsU()

g <- year0.25 %>% 
  make_plotsU()

(a + b  + d + g)

year2 <- year2 %>% 
  mutate(type = "two")
year1 <- year1 %>% 
  mutate(type = "one")
year0.5 <- year0.5 %>% 
  mutate(type = "half")
year0.25 <- year0.25 %>% 
  mutate(type = "quarter")

z <- 
  year2 %>% 
  full_join(year1) %>% 
  full_join(year0.5) %>% 
  full_join(year0.25) %>% 
  make_plots_tim_U()

z
