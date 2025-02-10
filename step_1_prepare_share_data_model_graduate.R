library(tidyverse)
library(haven)
# library(rio)
library(scales)
library(expm)
library(compositions)
library(expm)

gather_wave_data <- function(data,
                             dead    = "deadoralive_w9", 
                             age     = "age_int_w9", 
                             year    = "int_year_w9",
                             disease = "chronicw9") {
  
  # from generated file
  gen_files <- gen_files %>% 
    dplyr::select(mergeid,
                  country,
                  deceased_year,
                  deceased_age,
                  gender,
                  !!sym(dead),
                  !!sym(age),
                  !!sym(year)) %>%
    filter(country == 15) %>%
    filter(!!sym(year)> 0) %>%
    distinct() %>% 
    set_names(c("mergeid", "country", "deceased_year", 
                "deceased_age", "gender", 
                "dead", "age", "year"))
  
  # from health
  health  <- data %>%
    filter(country == 15) %>%
    dplyr::select(mergeid, gali, sphus, !!sym(disease)) %>% 
    rename("chronic" := !!sym(disease)) %>% 
    distinct()
  
  gen_files %>%
    left_join(health) %>%
    mutate(year = ifelse(deceased_year > 0, deceased_year, year)) %>%
    mutate(age  = ifelse(deceased_age  > 0, deceased_age,  age)) %>%
    dplyr::select(-c(country, deceased_year, deceased_age)) %>% 
    # filter(year %in% time) %>% !!!!!!!!!!!!!!!!!!!!!!
    filter(age > 0) %>%
    mutate(dead   = ifelse(dead == 2, "D", NA_character_)) %>% 
    mutate(gender = ifelse(gender == 2, "female", "male")) %>%
    mutate(gali = case_when(
      gali == 0 ~ "H",
      gali == 1 ~ "U",
      TRUE      ~ NA_character_
    )) %>%
    mutate(sphus = case_when(
      sphus %in% c(1, 2, 3) ~ "H",
      sphus %in% c(4, 5)    ~ "U",
      TRUE                  ~ NA_character_
    )) %>%
    mutate(chronic = case_when(
      chronic == 0 ~ "H",
      chronic > 0  ~ "U",
      TRUE  ~ NA_character_
    )) %>% 
    mutate(across(c(gali, sphus, chronic), ~ ifelse(is.na(.) & !is.na(dead), dead, .))) %>% 
    dplyr::select(-dead)
    # mutate(across(c(gali, sphus, chronic), ~ ifelse(. == "0", NA_character_, .))) 
  # %>% 
  #   mutate(gali      = ifelse(is.na(gali)    & !is.na(chronic), chronic, gali))    %>%
  #   mutate(gali      = ifelse(is.na(gali)    & !is.na(sphus),   sphus,   gali))    %>% 
  #   mutate(chronic   = ifelse(is.na(chronic) & !is.na(gali),    gali,    chronic)) %>% 
  #   mutate(chronic   = ifelse(is.na(chronic) & !is.na(sphus),   sphus,   chronic)) %>% 
  #   mutate(sphus     = ifelse(is.na(sphus)   & !is.na(gali),    gali,    sphus))   %>% 
  #   mutate(sphus     = ifelse(is.na(sphus)   & !is.na(sphus),   chronic, sphus))   %>%
  #   filter(!is.na(gali))
  
}

# make_dt <- function(.data, var = "sphus") { 
#   
#   .data %>%
#     # filter(mergeid == "ES-003038-01") %>% 
#     dplyr::select(id  = mergeid,
#                   sex = gender,
#                   age,
#                   time = year, 
#                   health := !!sym(var)) %>% 
#     group_by(id) %>%
#     mutate(n = n()) %>%
#     ungroup() %>%
#     filter(n > 1) %>% # remove people who are only shown once in the data
#     group_by(sex, id) %>%
#     # arrange by age
#     arrange(age) %>%
#     mutate(tst = ifelse(last(health == "D") & any(is.na(health)), 1, 0)) %>%
#     mutate(health = ifelse(tst == 1 & is.na(health), "U", health)) %>%
#     # input time t + 1 as time t if missing
#     fill(health, .direction = "down") %>%
#     # do vice versa
#     fill(health, .direction = "up") %>%
#     # now create to and from states
#     # for each ID
#     group_by(id) %>%
#     # arrange by are
#     arrange(age) %>%
#     # create from variable
#     rename(from = health) %>%
#     # create to variable with lead
#     mutate(to   = lead(from)) %>%
#     ungroup() %>%
#     # final imputation
#     mutate(to = ifelse(from == "D", "D", to)) %>%
#     # remove the people for which we know nothing
#     filter(!is.na(from)) %>%
#     mutate(to = ifelse(is.na(to) & !is.na(from), from, to)) %>%
#     # create period variable
#     dplyr::select(-c(n, tst)) %>% 
#     filter(age > 49)%>% 
#     filter(from != "D")
#   
# }




gen_files  <- read_sav("SHARE/sharewX_rel9-0-0_gv_allwaves_cv_r.sav")
health9    <- read_sav("SHARE/sharew9_rel9-0-0_gv_health.sav")
health8    <- read_sav("SHARE/sharew8_rel9-0-0_gv_health.sav")
health7    <- read_sav("SHARE/sharew7_rel9-0-0_gv_health.sav")
health6    <- read_sav("SHARE/sharew6_rel9-0-0_gv_health.sav")
health5    <- read_sav("SHARE/sharew5_rel9-0-0_gv_health.sav")
health4    <- read_sav("SHARE/sharew4_rel9-0-0_gv_health.sav")
# health3    <- read_sav("SHARE/sharew3_rel9-0-0_gv_health.sav") non existent
health2    <- read_sav("SHARE/sharew2_rel9-0-0_gv_health.sav")
health1    <- read_sav("SHARE/sharew1_rel9-0-0_gv_health.sav")



nine <- gather_wave_data(data     = health9,
                         dead     = "deadoralive_w9", 
                         age      = "age_int_w9", 
                         year     = "int_year_w9",
                         disease  = "chronicw9")

eight <- gather_wave_data(data    = health8,
                          dead    = "deadoralive_w8", 
                          age     = "age_int_w8", 
                          year    = "int_year_w8",
                          disease = "chronicw8")

seven <- gather_wave_data(data    = health7,
                          dead    = "deadoralive_w7", 
                          age     = "age_int_w7", 
                          year    = "int_year_w7",
                          disease = "chronicw7")

six <- gather_wave_data(data    = health6,
                        dead    = "deadoralive_w6", 
                        age     = "age_int_w6", 
                        year    = "int_year_w6",
                        disease = "chronicw6")

five <- gather_wave_data(data    = health5,
                         dead    = "deadoralive_w5", 
                         age     = "age_int_w5", 
                         year    = "int_year_w5",
                         disease = "chronicw5")

four <- gather_wave_data(data    = health4,
                         dead    = "deadoralive_w4", 
                         age     = "age_int_w4", 
                         year    = "int_year_w4",
                         disease = "chronicw4")

two <- gather_wave_data(data    = health2,
                        dead    = "deadoralive_w2", 
                        age     = "age_int_w2", 
                        year    = "int_year_w2",
                        disease = "chronicw2")

one <- gather_wave_data(data    = health1,
                        dead    = "deadoralive_w1", 
                        age     = "age_int_w1", 
                        year    = "int_year_w1",
                        disease = "chronicw1")

share <- one %>% 
  full_join(two) %>% 
  full_join(four) %>% 
  full_join(five) %>% 
  full_join(six) %>% 
  full_join(seven) %>% 
  full_join(eight) %>%
  full_join(nine) %>% 
  filter(!is.na(year))
  

# keep only transition of 2 year length
share1 <-  share %>% 
  filter(year %in% c(2004, 2007, 2011, 2013, 2015, 2017, 2019:2022)) %>% 
  filter(year %in% c(2011, 2013, 2015, 2017))



make_dt <- function(.data, var = "sphus") { 
  
  share1 %>%
    dplyr::select(id  = mergeid,
                  sex = gender,
                  age,
                  time = year, 
                  health := !!sym(var)) %>% 
    group_by(id) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n > 1) %>% # remove people who are only shown once in the data
  
    # create from variable
    rename(from = health) %>%
    group_by(id) %>% 
    arrange(age) %>% 
  
    # create to variable with lead
    mutate(to   = lead(from)) %>%
    ungroup() %>%
    # create period variable
    filter(age > 49) %>%
    filter(from != "D") %>% 
    filter(!is.na(to))
  
}


# sum up the initial entries to 2-year probs
self  <- share1 %>% 
  make_dt(var = "sphus")%>% 
  arrange(id, time, age) %>% 
  mutate(age2 = age - age %% 2,
         age3 = age %% 2) %>% 
  mutate(age = age2) %>% 
  dplyr::select(-c(age2, age3))

chron <- share1 %>% 
  make_dt(var = "chronic")%>% 
  arrange(id, time, age) %>% 
  mutate(age2 = age - age %% 2,
         age3 = age %% 2) %>% 
  mutate(age = age2) %>% 
  dplyr::select(-c(age2, age3))
  
gali  <- share1 %>% 
  make_dt(var = "gali")%>% 
  arrange(id, time, age) %>% 
  mutate(age2 = age - age %% 2,
         age3 = age %% 2) %>% 
  mutate(age = age2) %>% 
  dplyr::select(-c(age2, age3))


new_data <- expand_grid(age  = seq(50, 110, 2),
                        from = c("H", "U"),
                        time = sort(unique(self$time)),   # time measure
                        sex  = c("male", "female"))


# Calculate transition probabilities
# ----------------------------------------------------------------- #

probabilities <- function(.data) {
  
  tst <- .data %>%
    distinct() %>%
    # new weight
    count(time, sex, age, from, to) %>%
    group_nest(sex, from) %>%
    mutate(data = ifelse(
      from == "H",
      map(data, ~ .x %>%
            mutate(to = factor(
              to, levels = c("H", "U", "D")
            ))),
      map(data, ~ .x %>%
            mutate(to = factor(
              to, levels = c("U", "H", "D")
            )))
    )) %>%
    # the model itself
    mutate(model =  map(
      data,
      ~ nnet::multinom(
        to ~ age + time,
        #
        weights = n,
        data    = .x,
        trace = FALSE
      )
    )) %>%
    # predicted data. all this is to simply fit the new_data for transiton probabilities
    nest_join(new_data, by = c("sex", "from")) %>%
    mutate(predicted_data = map2(.x = model, .y = new_data, ~ predict(.x, .y, type = "probs"))) %>%
    mutate(finale = map2(.x = new_data, .y = predicted_data, ~ .x %>%
                           bind_cols(.y))) %>%
    dplyr::select(sex, from, finale) %>%
    unnest(finale) %>%
    group_nest(sex) %>%
    mutate(qxdata = map(
      data,
      ~ .x %>%
        pivot_longer(c(H, U, D), names_to  = "var", values_to = "val") %>%
        unite("trans", c(from, var), sep = "-") %>%
        pivot_wider(names_from  = trans, values_from = val)
    )) %>%
    dplyr::select(sex, qxdata)
  
  
  
  # calculate empirical transition probabilities
  # ----------------------------------------------------------------- #
  empiric <- .data %>%
    distinct() %>%
    # new weight
    count(time, sex, age, from, to) %>%
    group_by(sex, time, age, from) %>%
    reframe(to = to, prob_emp = n / sum(n)) %>%
    ungroup()
  
  return(lst(tst, empiric))
  
}

self_model <- self %>% 
  probabilities()

chronic_model <- chron %>% 
  probabilities()

gali_model <- gali %>% 
  probabilities()


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
  scale_x_continuous(breaks = seq(15, 85, 5)) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))


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
  scale_x_continuous(breaks = seq(15, 85, 5)) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))



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
  ggplot() +
  # geom_line(aes(x = age, y = prob, group = to, color = to), linewidth = 1) +
  geom_point(aes(x = age, y = prob_emp, color = to)) +
  facet_grid(from ~ sex, switch = "y") +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = seq(15, 85, 5)) +
  theme_light() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", face = "bold"))


self_mod  <- self_model$tst
self_emp  <- self_model$empiric
chron_mod <- chronic_model$tst
chron_emp <- chronic_model$empiric
gali_mod  <- gali_model$tst
gali_emp  <- gali_model$empiric

save(self_mod,  file = "Results/self_mod.RData")
save(self_emp,  file = "Results/self_emd.Rdata")
save(chron_mod, file = "Results/chronic_mod.RData")
save(chron_emp, file = "Results/chronic_emd.Rdata")
save(gali_mod,  file = "Results/gali_mod.RData")
save(gali_emp,  file = "Results/gali_emd.Rdata")


chronic     <- chron
self_health <- self

save(self_health, file = "Results/self_health.RData")
save(chronic, file = "Results/chronic.RData")
save(gali, file = "Results/gali.RData")




#######################################################
# interpolate
source("functions_grad.R")

# transition probabilities from the multinomial model
self1 <- self_mod %>%
  # filter(sex == "male") %>% # NOW for males | change to females if needed
  unnest(qxdata) %>%
  # filter(time > 2011) %>% 
  group_nest(sex, time) %>%
  mutate(data = map(data, ~ .x %>%
                      set_names(c(
                        "age", "HH", "HU", "HD", "UH", "UU", "UD"
                      )))) %>%
  mutate(U = map(data, ~ Ptibble2U_closed(
    .x, interval = 2, start_age = 50
  ))) %>%
  mutate(Q = map(U, ~ U2Q(.x))) %>%
  mutate(Rtibble = map(Q, ~ Q2Rtibble(.x))) %>%
  mutate(Rtibble1 = map(Rtibble, ~ graduate_Rtibble(.x,
                                                    interval_current = 2,
                                                    interval_desired = 1))) %>%
  mutate(Q1 = map(Rtibble1, ~ Rtibble2Q(.x, interval = 1,start_age = 50))) %>%
  mutate(U1 = map(Q1, ~ Q2U(t(.x), interval_desired = 1))) %>%
  mutate(Ptibble1 = map(U1, ~ U2Ptibble(.x, interval_current = 1))) %>% 
  dplyr::select(sex, time, Ptibble1) %>% 
  unnest(Ptibble1)

chron1 <- chron_mod %>%
  # filter(sex == "male") %>% # NOW for males | change to females if needed
  unnest(qxdata) %>%
  # filter(time > 2011) %>% 
  group_nest(sex, time) %>%
  mutate(data = map(data, ~ .x %>%
                      set_names(c(
                        "age", "HH", "HU", "HD", "UH", "UU", "UD"
                      )))) %>%
  mutate(U = map(data, ~ Ptibble2U_closed(
    .x, interval = 2, start_age = 50
  ))) %>%
  mutate(Q = map(U, ~ U2Q(.x))) %>%
  mutate(Rtibble = map(Q, ~ Q2Rtibble(.x))) %>%
  mutate(Rtibble1 = map(Rtibble, ~ graduate_Rtibble(.x,
                                                    interval_current = 2,
                                                    interval_desired = 1))) %>%
  mutate(Q1 = map(Rtibble1, ~ Rtibble2Q(.x, interval = 1,start_age = 50))) %>%
  mutate(U1 = map(Q1, ~ Q2U(t(.x), interval_desired = 1))) %>%
  mutate(Ptibble1 = map(U1, ~ U2Ptibble(.x, interval_current = 1))) %>% 
  dplyr::select(sex, time, Ptibble1) %>% 
  unnest(Ptibble1)

gali1 <- gali_mod %>%
  # filter(sex == "male") %>% # NOW for males | change to females if needed
  unnest(qxdata) %>%
  # filter(time > 2011) %>% 
  group_nest(sex, time) %>%
  mutate(data = map(data, ~ .x %>%
                      set_names(c(
                        "age", "HH", "HU", "HD", "UH", "UU", "UD"
                      )))) %>%
  mutate(U = map(data, ~ Ptibble2U_closed(
    .x, interval = 2, start_age = 50
  ))) %>%
  mutate(Q = map(U, ~ U2Q(.x))) %>%
  mutate(Rtibble = map(Q, ~ Q2Rtibble(.x))) %>%
  mutate(Rtibble1 = map(Rtibble, ~ graduate_Rtibble(.x,
                                                    interval_current = 2,
                                                    interval_desired = 1))) %>%
  mutate(Q1 = map(Rtibble1, ~ Rtibble2Q(.x, interval = 1,start_age = 50))) %>%
  mutate(U1 = map(Q1, ~ Q2U(t(.x), interval_desired = 1))) %>%
  mutate(Ptibble1 = map(U1, ~ U2Ptibble(.x, interval_current = 1))) %>% 
  dplyr::select(sex, time, Ptibble1) %>% 
  unnest(Ptibble1)
  


##### HERE IMPOSE 2 and 1 year
uu2 <-
self_mod %>%
  unnest(qxdata) %>%
  filter(time == 2013) %>%
  pivot_longer(-c(age,sex,time),names_to = "transition",values_to = "p") %>%
  mutate(transition = str_replace_all(transition, "-", ""))



uu <- self1 |>
  filter(time == 2013) %>%
  pivot_longer(-c(age,sex,time),names_to = "transition",values_to = "p")

uuu <-
  ggplot() +
  geom_line(data = uu,   aes(x=age,y=p,color=sex)) +
  geom_line(data = uu2,  aes(x=age,y=p,color=sex), lty = 3) +
  facet_wrap(~ transition) +
  theme(legend.position = "bottom") +
  theme_minimal()


ggsave(uuu, filename = "one_and_two_self.jpeg", scale = 2)

##########################################


# # 1) make U closed
# U <- Ptibble2U_closed(trns, interval = 2, start_age = 50)
# # 2) convert U to Q (rates) using U2Q()
# # U <- U[-nrow(U),-ncol(U)]
# Q <- U2Q(U)
# # 3) turn Q into a handy tibble using Q2Rtibble()
# Rtibble <- Q2Rtibble(Q) 
# # 4) now graduate the attrition
# Rtibble1 <- graduate_Rtibble(Rtibble, interval_current = 2, interval_desired = 1)
# # 5) take these single-age rates and convert back to Q using Rtibble2Q()
# Q1 <- Rtibble2Q(Rtibble1, interval = 1,start_age = 50)
# # 6) convert this back to U using Q2U()
# U1 <- Q2U(t(Q1), interval_desired = 1)
# # 7) convert U to a handy tibble using U2Ptibble()
# Ptibble1 <- U2Ptibble(U1, interval_current = 1)



self1 |>
  filter(time == 2013) %>% 
  pivot_longer(-c(age,sex,time),names_to = "transition",values_to = "p") |> 
  ggplot(aes(x=age,y=p,color=transition)) + 
  geom_line() +
  facet_wrap(~ sex) + 
  theme(legend.position = "bottom") + 
  theme_minimal()


chron1 |>
  filter(time == 2013) %>% 
  pivot_longer(-c(age,sex,time),names_to = "transition",values_to = "p") |> 
  ggplot(aes(x=age,y=p,color=transition)) + 
  geom_line() +
  facet_wrap(~ sex) + 
  theme(legend.position = "bottom") + 
  theme_minimal()


gali1 |>
  filter(time == 2013) %>% 
  pivot_longer(-c(age,sex,time),names_to = "transition",values_to = "p") |> 
  ggplot(aes(x=age,y=p,color=transition)) + 
  geom_line() +
  facet_wrap(~ sex) + 
  theme(legend.position = "bottom") + 
  theme_minimal()


extrap_lm_down <- function(y) {
  x    <- 50:112
  xnew <- data.frame(x = 19:49)
  y2   <-  predict(lm(y ~ x), newdata = xnew)
  c(y2, y)
}

extrapolate_type <- function(.data) {
  
  zz <- .data %>%
    group_nest(sex, time) %>% # test time, test age
    mutate(
      hh_alr = map(
        data,
        ~ .x %>%
          dplyr::select(age, starts_with("H")) %>%
          dplyr::select("HU", "HD", "HH") %>%
          as.matrix() %>%
          alr() %>%
          apply(2, extrap_lm_down)
      )
    ) %>%
    mutate(
      nh_alr = map(
        data,
        ~ .x %>%
          dplyr::select(age, starts_with("U")) %>%
          dplyr::select("UH", "UD", "UU") %>%
          as.matrix() %>%
          alr() %>%
          apply(2, extrap_lm_down)
      )
    ) %>%
    mutate(hh_inverse = map(
      hh_alr,
      ~ .x %>%
        alrInv()  %>%
        as.data.frame() %>%
        rename(`HH` = V3)
    )) %>%
    mutate(nh_inverse = map(
      nh_alr,
      ~ .x %>%
        alrInv()  %>%
        as.data.frame() %>%
        rename(`UU` = V3)
    ))
  
  return(zz)
  
}


extr_self <- self1 %>% 
  extrapolate_type()

extr_chron <- chron1 %>% 
  extrapolate_type()

extr_gali <- gali1 %>% 
  extrapolate_type()



extr_gali %>%
  dplyr::select(sex, time, nh_alr) %>%
  mutate(nh_alr = map(nh_alr, ~ .x %>%
                        as.data.frame())) %>%
  unnest(c(nh_alr)) %>%
  group_by(sex, time) %>%
  mutate(age = 19:112, .after = 2) %>%
  ungroup() %>%
  pivot_longer(-c(sex:age),
               names_to = "from_to",
               values_to = "p") %>%
  ggplot(aes(x = age, y = p, color = from_to)) +
  geom_line() +
  theme_bw() +
  facet_wrap(sex ~ time, labeller = label_wrap_gen(multi_line = TRUE), ncol = 7) +
  theme_bw()  +
  geom_vline(aes(xintercept = 87))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(face = "bold"),
        strip.text.x.top = element_text(face = "bold"))
# 
# 
# 
extr_self %>%
  dplyr::select(sex, time, nh_inverse) %>%
  unnest(c(nh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = 19:112, .after = 2) %>%
  ungroup() %>%
  pivot_longer(-c(sex:age),
               names_to = "from_to",
               values_to = "p") %>%
  ggplot(aes(x = age, y = p, color = from_to)) +
  geom_line() +
  theme_bw() +
  facet_wrap(sex ~ time, labeller = label_wrap_gen(multi_line = TRUE), ncol = 7) +
  theme_bw()  +
  scale_x_continuous(breaks =pretty_breaks())+
  geom_vline(aes(xintercept = 87))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(face = "bold"),
        strip.text.x.top = element_text(face = "bold"))


extr_self <- extr_self %>% 
  dplyr::select(sex, time, hh_inverse, nh_inverse) %>% 
  unnest(c(hh_inverse, nh_inverse)) %>%
  group_by(sex, time) %>% 
  mutate(age = 19:112) %>%
  ungroup()

extr_chron <- extr_chron %>% 
  dplyr::select(sex, time, hh_inverse, nh_inverse) %>% 
  unnest(c(hh_inverse, nh_inverse)) %>%
  group_by(sex, time) %>% 
  mutate(age = 19:112) %>%
  ungroup()

extr_gali <- extr_gali %>% 
  dplyr::select(sex, time, hh_inverse, nh_inverse) %>% 
  unnest(c(hh_inverse, nh_inverse)) %>%
  group_by(sex, time) %>% 
  mutate(age = 19:112) %>%
  ungroup()


extr_self %>% 
  pivot_longer(-c(sex, time, age),
               names_to = "from_to",
               values_to = "p") %>%
  ggplot(aes(x = age, y = p, color = from_to)) +
  geom_line() +
  theme_bw() +
  facet_wrap(sex ~ time, labeller = label_wrap_gen(multi_line = TRUE), ncol = 7) +
  theme_bw()  +
  geom_vline(aes(xintercept = 87))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(face = "bold"),
        strip.text.x.top = element_text(face = "bold"))
# 
  


save(extr_self,  file = "Results/extr_self.RData")
save(extr_chron, file = "Results/extr_chron.RData")
save(extr_gali,  file = "Results/extr_gali.RData")

