# ----------------------------------------------------------------- #
# source function
source("clean_code_2/0_packages.R")
source("clean_code_2/for_1_year/03_functions_adjust_trns_hmd_1year.R")

load("updated_results/graduated_prob.RData")
load("updated_results/smooth_hmd_qx.RData")
load("updated_results/Ra_one_year.RData")


load("updated_results/prev_self_1.RData")
load("updated_results/prev_gali_1.RData")
load("updated_results/prev_adl_1.RData")
load("updated_results/prev_iadl_1.RData")


one_year <- one_year %>% 
  filter(source == "analytic") %>% 
  dplyr::select(-source) %>% 
  unite("trns", c(from, to), sep = "") %>%
  pivot_wider(names_from  = trns,
              values_from = prob)

# load database

final_self  <- adjust(Ra         = Ra_one_year %>%
                        filter(health_var == "self_report"),
                      prev       = prev_self,
                      extrap_dat = one_year %>% 
                        filter(health_var == "self_report"))

final_gali  <- adjust(Ra         = Ra_one_year %>%
                        filter(health_var == "gali"),
                      prev       = prev_gali,
                      extrap_dat = one_year %>% 
                        filter(health_var == "gali")) 

final_adl  <- adjust(Ra          = Ra_one_year %>%
                       filter(health_var == "adl"),
                      prev       = prev_adl,
                      extrap_dat = one_year %>% 
                       filter(health_var == "adl")) 

final_iadl  <- adjust(Ra         = Ra_one_year %>%
                        filter(health_var == "iadl"),
                      prev       = prev_iadl,
                      extrap_dat = one_year %>% 
                        filter(health_var == "iadl")) 

final_self %>%
  filter(time == 2015) %>%
    # filter(age %in% c(50:110)) %>%
  ggplot() +
  geom_line(aes(x = age, y = val, color = to, lty = type), linewidth = 1) +
  facet_wrap(from ~ sex) +
  theme_light() +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))


final_gali %>%
  filter(time == 2013) %>%
  # filter(age %in% c(50:100)) %>%
  ggplot() +
  geom_line(aes(x = age, y = val, color = to, lty = type), linewidth = 1) +
  facet_wrap(from ~ sex) +
  theme_light() +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))

final_adl %>%
  filter(time == 2013) %>%
  # filter(age %in% c(50:100)) %>%
  ggplot() +
  geom_line(aes(x = age, y = val, color = to, lty = type), linewidth = 1) +
  facet_wrap(from ~ sex) +
  theme_light() +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))

final_iadl %>%
  filter(time == 2013) %>%
  # filter(age %in% c(50:100)) %>%
  ggplot() +
  geom_line(aes(x = age, y = val, color = to, lty = type), linewidth = 1) +
  facet_wrap(from ~ sex) +
  theme_light() +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.position = "bottom",
        strip.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black", face = "bold"))


# save
save(final_self,  file = "updated_results/final_self_trns_1.RData")
save(final_gali,  file = "updated_results/final_gali_trns_1.RData")
save(final_adl,   file = "updated_results/final_adl_trns_1.RData")
save(final_iadl,  file = "updated_results/final_iadl_trns_1.RData")


final_self  <- final_self  %>% 
  mutate(hvar = "self_report")
final_gali  <- final_gali  %>%
  mutate(hvar = "gali")
final_adl   <- final_adl   %>%
  mutate(hvar = "adl")
final_iadl  <- final_iadl  %>%
  mutate(hvar = "iadl")

final <- final_self %>% 
  full_join(final_gali) %>% 
  full_join(final_adl) %>% 
  full_join(final_iadl) %>% 
  # filter(between(age, 50, 100)) %>% 
  rename(prob = val,
         health_var = hvar,
         adjusted = type) %>% 
  mutate(adjusted = ifelse(adjusted == "Adjusted", "yes", "no")) %>% 
  dplyr::select(health_var, adjusted, sex, time, age, from, to, prob)

write.csv(final, file = gzfile("share_1_year_age_adj.csv.gz"))


save(final,  file = "updated_results/final_full_transitions_1.RData")
