
# -----------------------------------------------------------------------------#
# explain variable meaning
explain_names <- function(data) {
  tibble(name    = names(data), 
         meaning = map_chr(data, ~ attr(.x, "label")))
}

# -----------------------------------------------------------------------------#
gather_wave_data <- function(data = hlth[[3]],
                             age  = "age2011", 
                             wave = "w4") {
  
  disease <- str_c("chronic", wave)
  wave    <- str_c("deadoralive_" , wave)
  year    <- parse_number(age)
  year    <- c(year, year + 1)
  
  dead <- gen_files %>%
    dplyr::select(mergeid,
                  deceased_year,
                  deceased_age,
                  deadoralive_w9) %>% 
    filter(deadoralive_w9 == 2,
           deceased_year > 0,
           deceased_age  >= 0) %>% 
    set_names("mergeid", "deceased_year", "deceased_age", "deadoralive") %>% 
    filter(deceased_year %in% year) %>% #####
    dplyr::select(-deceased_year)
  
  sample <- gen_files %>%
    dplyr::select(mergeid,
                  gender,
                  contains(wave),
                  !!sym(age)) %>%
    mutate(year = min(year)) %>% 
    set_names("mergeid", "gender", "deadoralive", "age", "year") %>%
    filter(deadoralive %in% c(1, 2)) %>% 
    dplyr::select(-deadoralive)
  
  # not known weather he was dead or alive at wave 5
  # gen_files %>%
  #   filter(mergeid == "ES-092725-01") %>%
  #   view()
  
  generated <- sample %>%
    left_join(dead, by = "mergeid") %>%
    mutate(age         = ifelse(!is.na(deceased_age), deceased_age, age),
           deadoralive = ifelse(deadoralive == 2, "D", NA_character_),
           gender      = ifelse(gender == 1, "male", "female")) %>%
    dplyr::select(-deceased_age) %>%
    filter(age >= 0)
  
  health  <- data %>%
    rename("chronic" := !!sym(disease))
  
  
  health %>%
    left_join(generated, by = join_by(mergeid)) %>% 
    # prepare 3 health definitions
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
      chronic <  2  ~ "H",
      chronic >= 2  ~ "U",
      TRUE  ~ NA_character_
    )) %>%
    mutate(adl = case_when(
      adl == 0 ~ "H", 
      adl  > 0 ~ "U",
      TRUE ~ NA_character_
    )) %>% 
    mutate(iadl = case_when(
      iadl == 0 ~ "H", 
      iadl  > 0 ~ "U",
      TRUE ~ NA_character_
    )) %>%
    # if person has died, his health state is D
    mutate(across(
      c(gali, sphus, chronic, adl, iadl),
      ~ ifelse(!is.na(deadoralive), deadoralive, .)
    )) %>%
    dplyr::select(-deadoralive) %>% 
    # remove cases where none of the health states is known
    filter(!if_all(c(chronic, gali, sphus, adl, iadl), is.na))
  
  }

# -----------------------------------------------------------------------------#
make_dt <- function(.data, var) { 
  
  share1 %>%
    dplyr::select(id   = mergeid,
                  sex  = gender,
                  age,
                  time = year, 
                  # choose health definition
                  health := !!sym(var)) %>%
    group_by(id) %>%
    # how many transitions per person
    mutate(n = n()) %>%
    ungroup() %>%
    # remove people who are only shown once in the data (no transition)
    filter(n > 1) %>%
    # remove helper variable
    dplyr::select(-n) %>% 
    # create from variable
    rename(from = health) %>%
    filter(!is.na(from)) %>% 
    group_by(id) %>% 
    arrange(age) %>%
    # create to variable with lead
    mutate(to = lead(from)) %>%
    ungroup() %>%
    # keep only ages 50-100
    filter(between(age, 50, 100)) %>%
    # remove transitions from D to D
    filter(from != "D") %>%
    # remove people with unknown destination
    # NOTE: our transitions are at the beginning of interval
    # 2011 means transition at 2011-2013
    # so all the year 2017 is a part of 2015-2017
    # this there is no transition from 2017 onward
    # We hence hace 3 transitions
    filter(!is.na(to)) %>%
    arrange(id, time, age) %>%
    # make 2 year interval age
    mutate(age = age - age %% 2)
  
}
# -----------------------------------------------------------------------------#
probabilities <- function(.data) {
  
  tst <- .data %>%
    # calculate new weight
    count(time, sex, age, from, to) %>%
    group_nest(sex, from) %>%
    # create base health level for multinom. reg. denominator
    # it is always a self transition from H-H or U-U
    mutate(data = ifelse(from == "H", map(data, ~ .x %>%
                                            mutate(to = factor(
                                              to, levels = c("H", "U", "D")
                                            ))), 
                         # else if from == U
                         map(data, ~ .x %>%
                               mutate(to = factor(
                                 to, levels = c("U", "H", "D")
                               ))))) %>%
    # the model itself, weighted
    # additive effects of age and time, no smoothing (wiggly) 
    mutate(model1 =  map(data, ~ vgam(
      to ~ s(age, bs = "ps") + time,
      weights = n,
      data    = .x,
      family  = multinomial
    )),
    model2 =  map(data, ~ vgam(
      to ~ age + time,
      weights = n,
      data    = .x,
      family  = multinomial
    )),
    aic1 = map_dbl(model1, AIC),
    bic1 = map_dbl(model1, BIC),
    aic2 = map_dbl(model2, AIC),
    bic2 = map_dbl(model2, BIC)) %>%
    # predicted data. 
    # all this is to fit predict the new_data for transition probabilities
    nest_join(new_data, by = c("sex", "from")) %>%
    mutate(predicted_data = map2(.x = model2, 
                                 .y = new_data, ~
                                   predict(.x, .y, type = "response"))) %>%
    mutate(finale = map2(.x = new_data, 
                         .y = predicted_data, ~ .x %>%
                           bind_cols(.y))) %>%
    dplyr::select(sex, from, finale) %>% 
    unnest(finale) %>%
    # predicted data. all this is to simply fit the new_data for transiton probabilities
    group_nest(sex) %>%
    # small data reformat, nothing fundamental, only wrangling
    mutate(qxdata = map(
      data,
      ~ .x %>%
        pivot_longer(c(H, U, D), 
                     names_to  = "var", 
                     values_to = "val") %>%
        unite("trans", c(from, var), sep = "-") %>%
        pivot_wider(names_from  = trans, 
                    values_from = val)
    )) %>%
    dplyr::select(sex, qxdata)
  
  # calculate empirical transition probabilities
  # ----------------------------------------------------------------- #
  empiric <- .data %>%
    # new weight
    count(time, sex, age, from, to) %>%
    group_by(sex, time, age, from) %>%
    # empirical probabilities
    reframe(to = to, 
            prob_emp = n / sum(n)) %>%
    ungroup()
  
  return(lst(tst, empiric))
  
}
