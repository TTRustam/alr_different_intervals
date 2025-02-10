
load("SILC/final_adj_raw.RData")
load("Results/final_share.RData")


z <- final %>% 
  mutate(panel = "SILC")%>%
  filter(adjusted == "Adjusted") %>% 
  dplyr::select(-adjusted)


z1 <- final_share %>% 
  mutate(panel = "SHARE") 

# I think this is the best one.
uu <- z %>% 
  full_join(z1) %>%
  filter(between(age, 50, 85)) %>%
  filter(sex == "male") %>%
  unite(trans, c("from", "to"), sep = "") %>%
  # filter(trans %in% c("HU", "UD")) %>%
  filter(year %in% unique(z1$year)) %>% 
  ggplot(aes(x = age, y = p, color = trans, lty = panel)) + 
  geom_line(linewidth = 1) + 
  facet_grid(year ~ type, switch = "y") + 
  theme_minimal() + 
  theme(legend.position = "bottom")

uu1 <- z %>% 
  full_join(z1) %>%
  filter(between(age, 50, 85)) %>%
  filter(sex == "male") %>%
  unite(trans, c("from", "to"), sep = "") %>%
  # filter(trans %in% c("HU", "UD")) %>%
  filter(year %in% unique(z1$year)) %>% 
  ggplot(aes(x = age, y = p, color = trans, lty = panel)) + 
  geom_line(linewidth = 1) + 
  scale_y_log10()+
  facet_grid(year ~ type, switch = "y") + 
  theme_minimal() + 
  theme(legend.position = "bottom")



  # ggsave(uu, filename = "new_silc_share_m.jpeg",scale = 2)
  # ggsave(uu1, filename = "new_silc_share_m_log.jpeg",scale = 2)
  # 


final_data <- z %>% 
  full_join(z1) %>% 
  filter(year %in% unique(z1$year)) %>% 
  filter(!is.na(p))



save(final_data, file = "Results/final_result.RData")
