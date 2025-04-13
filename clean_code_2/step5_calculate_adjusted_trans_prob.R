# ----------------------------------------------------------------- #
# source function
source("clean_code_2/0_packages.R")
source("clean_code_2/05_functions_adjust_trns_by_HMD.R")

# load database
load("updated_results/Ra_self_2.RData")
load("updated_results/prev_self_2.RData")
load("updated_results/inter_self_2.RData")

load("updated_results/Ra_chron_2.RData")
load("updated_results/prev_chron_2.RData")
load("updated_results/inter_chron_2.RData")

load("updated_results/Ra_gali_2.RData")
load("updated_results/prev_gali_2.RData")
load("updated_results/inter_gali_2.RData")

load("updated_results/Ra_adl_2.RData")
load("updated_results/prev_adl_2.RData")
load("updated_results/inter_adl_2.RData")

load("updated_results/Ra_iadl_2.RData")
load("updated_results/prev_iadl_2.RData")
load("updated_results/inter_iadl_2.RData")



final_self  <- adjust(Ra         = Ra_self,
                      prev       = prev_self,
                      extrap_dat = extr_self)

final_chron <- adjust(Ra         = Ra_chron,
                      prev       = prev_chron,
                      extrap_dat = extr_chron) 

final_gali  <- adjust(Ra         = Ra_gali,
                      prev       = prev_gali,
                      extrap_dat = extr_gali) 

final_adl  <- adjust(Ra          = Ra_adl,
                      prev       = prev_adl,
                      extrap_dat = extr_adl) 

final_iadl  <- adjust(Ra         = Ra_iadl,
                      prev       = prev_iadl,
                      extrap_dat = extr_iadl) 

final_self %>%
  filter(time == 2013) %>%
    filter(age %in% c(50:100)) %>%
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

final_chron %>%
  filter(time == 2013) %>%
  filter(age %in% c(50:100)) %>%
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
  filter(age %in% c(50:100)) %>%
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
  filter(age %in% c(50:100)) %>%
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
  filter(age %in% c(50:100)) %>%
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
save(final_self,  file = "updated_results/final_self_trns_2.RData")
save(final_chron, file = "updated_results/final_chron_trns_2.RData")
save(final_gali,  file = "updated_results/final_gali_trns_2.RData")
save(final_adl,   file = "updated_results/final_adl_trns_2.RData")
save(final_iadl,  file = "updated_results/final_iadl_trns_2.RData")


final_self  <- final_self  %>% 
  mutate(hvar = "self_report")
final_chron <- final_chron %>%
  mutate(hvar = "chronic")
final_gali  <- final_gali  %>%
  mutate(hvar = "gali")
final_adl   <- final_adl   %>%
  mutate(hvar = "adl")
final_iadl  <- final_iadl  %>%
  mutate(hvar = "iadl")
  
final <- final_self %>% 
  full_join(final_chron) %>% 
  full_join(final_gali) %>% 
  full_join(final_adl) %>% 
  full_join(final_iadl) %>% 
  filter(between(age, 50, 100)) %>% 
  rename(prob = val,
         health_var = hvar,
         adjusted = type) %>% 
  mutate(adjusted = ifelse(adjusted == "Adjusted", "yes", "no")) %>% 
  dplyr::select(health_var, adjusted, sex, time, age, from, to, prob)

write.csv(final, file = gzfile("share_2_year_age_adj.csv.gz"))


# save(final,  file = "updated_results/final_full_transitions_2.RData")
