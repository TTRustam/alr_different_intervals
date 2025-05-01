# -----------------------------------------------------------------------------#
source("clean_code_2/0_packages.R")
source("clean_code_2/01_SHARE_trans_prob_functions.R")
# -----------------------------------------------------------------------------#
# health variables
health <- c(
  "SHARE/health/sharew1_rel9-0-0_gv_health.sav",
  "SHARE/health/sharew2_rel9-0-0_gv_health.sav",
  "SHARE/health/sharew4_rel9-0-0_gv_health.sav",
  "SHARE/health/sharew5_rel9-0-0_gv_health.sav",
  "SHARE/health/sharew6_rel9-0-0_gv_health.sav",
  "SHARE/health/sharew7_rel9-0-0_gv_health.sav",
  "SHARE/health/sharew8_rel9-0-0_gv_health.sav",
  "SHARE/health/sharew9_rel9-0-0_gv_health.sav"
)

# read health all data
hlth <- map(
  health,
  ~ read_sav(.x, col_select = c(
    country, mergeid, gali, sphus, adl, iadl, num_range("chronicw", 1:9)
  )) %>%
    filter(country == 15) %>%  # Keep only Spain
    select(-country)           # Drop country column since it's now redundant
)

# -----------------------------------------------------------------------------#
# death info and sex from generated files
gen_files  <- read_sav("SHARE/sharewX_rel9-0-0_gv_allwaves_cv_r.sav",
                       col_select = c(mergeid,
                                      country,
                                      deceased_year,
                                      deceased_month,
                                      deceased_age,
                                      num_range("age_int_w", range = 1:9),
                                      num_range("int_month_w", range = 1:9),
                                      num_range("int_year_w", range = 1:9),
                                      num_range("deadoralive_w", range = 1:9),
                                      gender,
                                      starts_with("age20"))) %>% 
  filter(country == 15) %>% 
  dplyr::select(-country)


# choose vars from different waves
wave_vars <- tibble(
  # age = c(
  #   "age2004",
  #   "age2007",
  #   "age2011",
  #   "age2013",
  #   "age2015",
  #   "age2017",
  #   "age2020",
  #   "age2021"
  # ),
  wave = c(
    "w1",
    "w2",
    "w4",
    "w5",
    "w6",
    "w7",
    "w8",
    "w9"
    )
)

# Use pmap() to iterate over datasets and corresponding variable names
# iteratively binding the generated sample to corresponding health dta
hlth_processed <- pmap(
  list(data = hlth, 
       # age  = wave_vars$age, 
       wave = wave_vars$wave),
  gather_wave_data
) %>% 
  set_names(c("w1",
              "w2",
              "w4",
              "w5",
              "w6",
              "w7",
              "w8",
              "w9"))

# -----------------------------------------------------------------------------#
share1 <- hlth_processed[c("w4", "w5", "w6", "w7")] %>%
  bind_rows()

# initial entries to 2 year data from 3 health definitions
# change in function 100 to 90
self  <- share1 %>% 
  make_dt(var = "sphus")
chron <- share1 %>%
  make_dt(var = "chronic")
gali  <- share1 %>% 
  make_dt(var = "gali")
adl  <- share1 %>% 
  make_dt(var = "adl")
iadl  <- share1 %>% 
  make_dt(var = "iadl")

self %>%
  count(age) %>%
  filter(n < 3)
chron %>%
  count(age) %>%
  filter(n < 3)
gali %>%
  count(age) %>%
  filter(n < 3)
adl %>%
  count(age) %>%
  filter(n < 3)
iadl %>%
  count(age) %>%
  filter(n < 3)


# --------------------------------------- #
# for Tim
# self  <- share1 %>% 
#   make_dt(var = "sphus") %>% 
#   count(sex, time, age, from, to) %>% 
#   mutate(condition = "sphus")
# 
# chron <- share1 %>%
#   make_dt(var = "chronic")%>%
#   count(sex, time, age, from, to) %>%
#   mutate(condition = "chronic")
# 
# gali  <- share1 %>% 
#   make_dt(var = "gali") %>% 
#   count(sex, time, age, from, to) %>%  
#   mutate(condition = "gali")
# 
# adl  <- share1 %>% 
#   make_dt(var = "adl") %>% 
#   count(sex, time, age, from, to) %>%
#   mutate(condition = "adl")
# 
# iadl  <- share1 %>% 
#   make_dt(var = "iadl") %>%
#   count(sex, time, age, from, to) %>%
#   mutate(condition = "iadl")
# 
# 
# 
# emp_prevalence <- self %>%
#   full_join(chron) %>% 
#   full_join(gali) %>% 
#   full_join(adl) %>% 
#   full_join(gali) %>% 
#   full_join(iadl)
# 
# save(emp_prevalence, file = "updated_results/empiric_prev.RData")
# --------------------------------------- #



# -----------------------------------------------------------------------------#
# data for fitting
new_data <- expand_grid(age  = seq(50, 100, 2),
                        from = c("H", "U"),
                        time = sort(unique(self$time)),   # time measure
                        sex  = c("male", "female"))

# Calculate 2 year transition probabilities with multinomial reg.
# I tried smoothing. Line becomes wiggly, with no apparent advantages
# so I kept linear trend
self_model <- self %>% 
  probabilities() # probabilities_no_time
chronic_model <- chron %>% 
  probabilities() # probabilities_no_time
gali_model <- gali %>% 
  probabilities() # probabilities_no_time
adl_model <- adl %>% 
  probabilities() # probabilities_no_time
iadl_model <- iadl %>% 
  probabilities() # probabilities_no_time
# -----------------------------------------------------------------------------#
# plots empirical vs fitted
# self looks ok
self_model$tst %>%
  unnest(qxdata) %>%
  pivot_longer(-c(sex:time),
               names_to = "trans",
               values_to = "prob") %>%
  separate(trans, into = c("from", "to"), sep = "-") %>%
  full_join(self_model$empiric) %>%
  mutate(prob_emp = ifelse(prob_emp == 1, NA, prob_emp)) %>%
  filter(time == 2013) %>% # change years here.
  ggplot() +
  geom_line(aes(x = age, y = prob, group = to, color = to), linewidth = 1) +
  geom_point(aes(x = age, y = prob_emp, color = to)) +
  facet_grid(from ~ sex, switch = "y") +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = seq(15, 110, 5)) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))

# chronic looks ok too
chronic_model$tst %>%
  unnest(qxdata) %>%
  pivot_longer(-c(sex:time),
               names_to = "trans",
               values_to = "prob") %>%
  separate(trans, into = c("from", "to"), sep = "-") %>%
  full_join(chronic_model$empiric) %>%
  # remove empirical values that are == 1, for better visualization
  # I think it is arefact of the data rather than really important values
  mutate(prob_emp = ifelse(prob_emp == 1, NA, prob_emp)) %>%
  filter(time == 2013) %>% # change years here.
  ggplot() +
  geom_line(aes(x = age, y = prob, group = to, color = to), linewidth = 1) +
  geom_point(aes(x = age, y = prob_emp, color = to)) +
  facet_grid(from ~ sex, switch = "y") +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = seq(15, 110, 5)) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))

# gali looks ok too
# in fact trends are somewhat linear here
gali_model$tst %>%
  unnest(qxdata) %>%
  pivot_longer(-c(sex:time),
               names_to = "trans",
               values_to = "prob") %>%
  separate(trans, into = c("from", "to"), sep = "-") %>%
  full_join(gali_model$empiric) %>%
  # remove empirical values that are == 1, for better visualization
  # I think it is arefact of the data rather than really important values
  mutate(prob_emp = ifelse(prob_emp == 1, NA, prob_emp)) %>%
  filter(time == 2013) %>% # change years here.
  filter(age < 85) %>% 
  ggplot() +
  geom_line(aes(x = age, y = prob, group = to, color = to), linewidth = 1) +
  geom_point(aes(x = age, y = prob_emp, color = to)) +
  facet_grid(from ~ sex, switch = "y") +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = seq(15, 110, 5)) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))

adl_model$tst %>%
  unnest(qxdata) %>%
  pivot_longer(-c(sex:time),
               names_to = "trans",
               values_to = "prob") %>%
  separate(trans, into = c("from", "to"), sep = "-") %>%
  full_join(adl_model$empiric) %>%
  mutate(prob_emp = ifelse(prob_emp == 1, NA, prob_emp)) %>%
  filter(time == 2013) %>% # change years here.
  ggplot() +
  geom_line(aes(x = age, y = prob, group = to, color = to), linewidth = 1) +
  geom_point(aes(x = age, y = prob_emp, color = to)) +
  facet_grid(from ~ sex, switch = "y") +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = seq(15, 110, 5)) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))

iadl_model$tst %>%
  unnest(qxdata) %>%
  pivot_longer(-c(sex:time),
               names_to = "trans",
               values_to = "prob") %>%
  separate(trans, into = c("from", "to"), sep = "-") %>%
  full_join(iadl_model$empiric) %>%
  mutate(prob_emp = ifelse(prob_emp == 1, NA, prob_emp)) %>%
  filter(time == 2013) %>% # change years here.
  ggplot() +
  geom_line(aes(x = age, y = prob, group = to, color = to), linewidth = 1) +
  geom_point(aes(x = age, y = prob_emp, color = to)) +
  facet_grid(from ~ sex, switch = "y") +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = seq(15, 110, 5)) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))

# test
self_model$tst %>%
  unnest(qxdata) %>% 
  dplyr::select(-c(sex, age, time)) %>%
  (`<=`)(0) %>% 
  colSums()

chronic_model$tst %>%
  unnest(qxdata) %>% 
  dplyr::select(-c(sex, age, time)) %>%
  (`<=`)(0) %>% 
  colSums()

gali_model$tst %>%
  unnest(qxdata) %>% 
  dplyr::select(-c(sex, age, time)) %>%
  (`<=`)(0) %>% 
  colSums()

adl_model$tst %>%
  unnest(qxdata) %>% 
  dplyr::select(-c(sex, age, time)) %>%
  (`<=`)(0) %>% 
  colSums()

iadl_model$tst %>%
  unnest(qxdata) %>% 
  dplyr::select(-c(sex, age, time)) %>%
  (`<=`)(0) %>% 
  colSums()

iadl_model$tst %>%
  unnest(qxdata) %>% 
  dplyr::select(-c(sex, age, time)) %>% 
  rowSums()



self_mod  <- self_model$tst
self_emp  <- self_model$empiric

chron_mod <- chronic_model$tst
chron_emp <- chronic_model$empiric

gali_mod  <- gali_model$tst
gali_emp  <- gali_model$empiric

adl_mod   <- adl_model$tst
adl_emp   <- adl_model$empiric

iadl_mod  <- iadl_model$tst
iadl_emp  <- iadl_model$empiric

save(self,  file = "updated_results/self_health.RData")
save(chron, file = "updated_results/chronic.RData")
save(gali,  file = "updated_results/gali.RData")
save(adl,   file = "updated_results/adl.RData")
save(iadl,  file = "updated_results/iadl.RData")

save(self_mod,    file = "updated_results/self_mod.RData")
save(self_emp,    file = "updated_results/self_emp.Rdata")
save(chron_mod,   file = "updated_results/chronic_mod.RData")
save(chron_emp,   file = "updated_results/chronic_emp.Rdata")
save(gali_mod,    file = "updated_results/gali_mod.RData")
save(gali_emp,    file = "updated_results/gali_emp.Rdata")
save(adl_mod,     file = "updated_results/adl_mod.RData")
save(adl_emp,     file = "updated_results/adl_emp.Rdata")
save(iadl_mod,    file = "updated_results/iadl_mod.RData")
save(iadl_emp,    file = "updated_results/iadl_emp.Rdata")


