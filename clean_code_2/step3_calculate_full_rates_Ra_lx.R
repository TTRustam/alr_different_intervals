# -----------------------------------------------------------------------------#
# source function
source("clean_code_2/0_packages.R")
# load database
load("updated_results/inter_self_2.RData")
load("updated_results/inter_chron_2.RData")
load("updated_results/inter_gali_2.RData")
load("updated_results/inter_adl_2.RData")
load("updated_results/inter_iadl_2.RData")
# -----------------------------------------------------------------------------#
# calculate lifetable. keep ages 20-110
calculate_Ra <- function(.data) { 
  
  .data %>% 
    group_by(sex, time) %>% 
    mutate(mh = HD / (2 - HD),
           mu = UD / (2 - UD)) %>% 
    mutate(Ra = mu / mh)
  
}


lets <- function(.data) {
  
  for(i in 1:length(.data$lu[-1])) {
    
    .data$lh[i + 1] <-
      .data$lh[i] * .data$`HH`[i] + .data$lu[i] * .data$`UH`[i]
    
    .data$lu[i + 1] <-
      .data$lu[i] * .data$`UU`[i] + .data$lh[i] * .data$`HU`[i]
    
  }
  
  return(.data)
  
}



# rates and Ra
lt_self <-  extr_self %>% 
  calculate_Ra() %>% 
  group_by(sex, time) %>%
  mutate(lh = 0, lu = 0) %>%
  mutate(lh = c(1, lh[-1])) %>%
  group_nest() %>%
  mutate(data = map(data, ~ .x %>%
                      lets())) %>% 
  unnest(data)

lt_chron <- extr_chron %>% 
  calculate_Ra() %>% 
  group_by(sex, time) %>%
  mutate(lh = 0, lu = 0) %>%
  mutate(lh = c(1, lh[-1])) %>%
  group_nest() %>%
  mutate(data = map(data, ~ .x %>%
                      lets())) %>% 
  unnest(data)

lt_gali <- extr_gali %>% 
  calculate_Ra() %>% 
  group_by(sex, time) %>%
  mutate(lh = 0, lu = 0) %>%
  mutate(lh = c(1, lh[-1])) %>%
  group_nest() %>%
  mutate(data = map(data, ~ .x %>%
                      lets())) %>% 
  unnest(data)

lt_adl <- extr_adl %>% 
  calculate_Ra() %>% 
  group_by(sex, time) %>%
  mutate(lh = 0, lu = 0) %>%
  mutate(lh = c(1, lh[-1])) %>%
  group_nest() %>%
  mutate(data = map(data, ~ .x %>%
                      lets())) %>% 
  unnest(data)

lt_iadl <- extr_iadl %>% 
  calculate_Ra() %>% 
  group_by(sex, time) %>%
  mutate(lh = 0, lu = 0) %>%
  mutate(lh = c(1, lh[-1])) %>%
  group_nest() %>%
  mutate(data = map(data, ~ .x %>%
                      lets())) %>% 
  unnest(data)


# save Ra
Ra_self <- lt_self %>% 
  dplyr::select(sex, time, age, Ra, starts_with("m"), ends_with("D"))

Ra_chron <- lt_chron %>% 
  dplyr::select(sex, time, age, Ra, starts_with("m"), ends_with("D"))

Ra_gali <- lt_gali %>% 
  dplyr::select(sex, time, age, Ra, starts_with("m"), ends_with("D"))

Ra_adl <- lt_adl %>% 
  dplyr::select(sex, time, age, Ra, starts_with("m"), ends_with("D"))

Ra_iadl <- lt_iadl %>% 
  dplyr::select(sex, time, age, Ra, starts_with("m"), ends_with("D"))


# tests mortality rates
lt_self %>% 
  dplyr::select(sex, time, age, mu, mh) %>% 
  mutate(time = as.factor(time)) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "mx",
               values_to = "val") %>% 
  ggplot(aes(x = age, y = val, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(mx~  sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

lt_chron %>% 
  dplyr::select(sex, time, age, mu, mh) %>% 
  mutate(time = as.factor(time)) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "mx",
               values_to = "val") %>% 
  ggplot(aes(x = age, y = val, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(mx~  sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

lt_gali %>% 
  dplyr::select(sex, time, age, mu, mh) %>% 
  mutate(time = as.factor(time)) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "mx",
               values_to = "val") %>% 
  ggplot(aes(x = age, y = val, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(mx~  sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")


lt_adl %>% 
  dplyr::select(sex, time, age, mu, mh) %>% 
  mutate(time = as.factor(time)) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "mx",
               values_to = "val") %>% 
  ggplot(aes(x = age, y = val, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(mx~  sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")


lt_iadl %>% 
  dplyr::select(sex, time, age, mu, mh) %>% 
  mutate(time = as.factor(time)) %>% 
  pivot_longer(-c(sex, time, age),
               names_to  = "mx",
               values_to = "val") %>% 
  ggplot(aes(x = age, y = val, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(mx~  sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

# test RA. Actually I do not like what I see
lt_self %>% 
  dplyr::select(sex, time, age, Ra) %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(~ sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

lt_chron %>% 
  dplyr::select(sex, time, age, Ra) %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(~ sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

lt_gali %>% 
  dplyr::select(sex, time, age, Ra) %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(~ sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

lt_adl %>% 
  dplyr::select(sex, time, age, Ra) %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(~ sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

lt_iadl %>% 
  dplyr::select(sex, time, age, Ra) %>% 
  mutate(time = as.factor(time)) %>% 
  ggplot(aes(x = age, y = Ra, color = time)) + 
  geom_line() + 
  scale_y_log10()+
  facet_wrap(~ sex) + 
  theme_minimal() + 
  theme(legend.position = "bottom")
# -----------------------------------------------------------------------------#

save(lt_self,  file = "updated_results/lt_self_2.RData")
save(lt_chron, file = "updated_results/lt_chron_2.RData")
save(lt_gali,  file = "updated_results/lt_gali_2.RData")
save(lt_adl,   file = "updated_results/lt_adl_2.RData")
save(lt_iadl,  file = "updated_results/lt_iadl_2.RData")


save(Ra_self,  file = "updated_results/Ra_self_2.RData")
save(Ra_chron, file = "updated_results/Ra_chron_2.RData")
save(Ra_gali,  file = "updated_results/Ra_gali_2.RData")
save(Ra_adl,   file = "updated_results/Ra_adl_2.RData")
save(Ra_iadl,  file = "updated_results/Ra_iadl_2.RData")

# -----------------------------------------------------------------------------#
# diagnostic plot lh lu

lt_self %>% 
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

lt_chron %>% 
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


lt_gali %>% 
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

lt_adl %>% 
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



lt_iadl %>% 
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


(lt_self$lh + lt_self$lu)[1]
(lt_chron$lh + lt_chron$lu)[1]
(lt_gali$lh + lt_gali$lu)[1]
(lt_adl$lh + lt_adl$lu)[1]
(lt_iadl$lh + lt_iadl$lu)[1]

