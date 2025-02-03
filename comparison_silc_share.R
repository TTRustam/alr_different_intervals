
load("SILC/final.RData")
load("Results/final_share.RData")


z <- final %>% 
  mutate(panel = "SILC")


z1 <- final_share %>% 
  mutate(panel = "SHARE") %>%
  dplyr::select(-var)


# I think this is the best one.
z %>% 
  full_join(z1) %>%
  filter(age > 19) %>%
  filter(type == "gali", year == 2013) %>%
  dplyr::select(-year, -type)%>%
  ggplot(aes(x = age, y = p, color = to, lty = panel)) + 
  geom_line(linewidth = 1) + 
  facet_grid(sex ~ from) + 
  theme_minimal() + 
  theme(legend.position = "bottom")

z %>% 
  full_join(z1) %>%
  filter(age > 20) %>%
  filter(type == "chronic", year == 2013) %>%
  dplyr::select(-year, -type)%>%
  ggplot(aes(x = age, y = p, color = to, lty = panel)) + 
  geom_line(linewidth = 1) + 
  facet_grid(sex ~ from) + 
  theme_minimal() + 
  theme(legend.position = "bottom")



z %>% 
  full_join(z1) %>%
  filter(age > 20) %>%
  filter(type == "self_rated", year == 2013) %>%
  dplyr::select(-year, -type)%>%
  ggplot(aes(x = age, y = p, color = to, lty = panel)) + 
  geom_line(linewidth = 1) + 
  facet_grid(sex ~ from) + 
  theme_minimal() + 
  theme(legend.position = "bottom")



final_data <- z %>% 
  full_join(z1)



save(final_data, file = "Results/final_result.RData")
