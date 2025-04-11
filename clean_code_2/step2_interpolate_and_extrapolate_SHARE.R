# -----------------------------------------------------------------------------#
# extrapolation
# -----------------------------------------------------------------------------#
source("clean_code_2/0_packages.R")
source("clean_code_2/02_functions_grad.R")

load("updated_results/self_mod.RData")
load("updated_results/self_emp.Rdata")

load("updated_results/chronic_mod.RData")
load("updated_results/chronic_emp.Rdata")

load("updated_results/gali_mod.RData")
load("updated_results/gali_emp.Rdata")

load("updated_results/adl_mod.RData")
load("updated_results/adl_emp.Rdata")

load("updated_results/iadl_mod.RData")
load("updated_results/iadl_emp.Rdata")

# -----------------------------------------------------------------------------#
# first we extrapolate 2 year transition probabilities
# from 50:90 to 20:110
extr_self <- self_mod %>%
  extrapolate_type()

extr_chron <- chron_mod %>%
  extrapolate_type()

extr_gali <- gali_mod %>%
  extrapolate_type()

extr_adl <- adl_mod %>%
  extrapolate_type()

extr_iadl <- iadl_mod %>%
  extrapolate_type()
# -----------------------------------------------------------------------------#
extr_self %>%
  dplyr::select(sex, time, hh_inverse) %>%
  unnest(c(hh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
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
  geom_vline(aes(xintercept = 50))+
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

extr_chron %>%
  dplyr::select(sex, time, hh_inverse) %>%
  unnest(c(hh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
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
  geom_vline(aes(xintercept = 50))+
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

extr_gali %>%
  dplyr::select(sex, time, hh_inverse) %>%
  unnest(c(hh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
  ungroup() %>%
  pivot_longer(-c(sex:age),
               names_to  = "from_to",
               values_to = "p") %>%
  ggplot(aes(x = age, y = p, color = from_to)) +
  geom_line() +
  theme_bw() +
  facet_wrap(sex ~ time, labeller = label_wrap_gen(multi_line = TRUE), ncol = 7) +
  theme_bw()  +
  scale_x_continuous(breaks =pretty_breaks())+
  geom_vline(aes(xintercept = 50))+
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

extr_adl %>%
  dplyr::select(sex, time, nh_inverse) %>%
  unnest(c(nh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
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
  geom_vline(aes(xintercept = 50))+
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

extr_iadl %>%
  dplyr::select(sex, time, hh_inverse) %>%
  unnest(c(hh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
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
  geom_vline(aes(xintercept = 50))+
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

# -----------------------------------------------------------------------------#
# save in nice format
extr_self <- extr_self %>%
  dplyr::select(sex, time, hh_inverse, nh_inverse) %>%
  unnest(c(hh_inverse, nh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
  ungroup()

extr_chron <- extr_chron %>%
  dplyr::select(sex, time, hh_inverse, nh_inverse) %>%
  unnest(c(hh_inverse, nh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
  ungroup()

extr_gali <- extr_gali %>%
  dplyr::select(sex, time, hh_inverse, nh_inverse) %>%
  unnest(c(hh_inverse, nh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
  ungroup()

extr_adl <- extr_adl %>%
  dplyr::select(sex, time, hh_inverse, nh_inverse) %>%
  unnest(c(hh_inverse, nh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
  ungroup()

extr_iadl <- extr_iadl %>%
  dplyr::select(sex, time, hh_inverse, nh_inverse) %>%
  unnest(c(hh_inverse, nh_inverse)) %>%
  group_by(sex, time) %>%
  mutate(age = seq(from = 20, to = 110, by = 2), .after = 2) %>%
  ungroup()

# -----------------------------------------------------------------------------#
# tests

save(extr_self,  file = "updated_results/inter_self_2.RData")
save(extr_chron, file = "updated_results/inter_chron_2.RData")
save(extr_gali,  file = "updated_results/inter_gali_2.RData")
save(extr_adl,   file = "updated_results/inter_adl_2.RData")
save(extr_iadl,  file = "updated_results/inter_iadl_2.RData")



# extr_self %>% 
#   dplyr::select(-c(sex, time, age)) %>% 
#   rowSums()
# 
# extr_chron %>% 
#   dplyr::select(-c(sex, time, age)) %>% 
#   rowSums()
# 
# extr_gali %>% 
#   dplyr::select(-c(sex, time, age)) %>% 
#   rowSums()
# 
# extr_adl %>% 
#   dplyr::select(-c(sex, time, age)) %>% 
#   rowSums()
# 
# extr_iadl %>% 
#   dplyr::select(-c(sex, time, age)) %>% 
#   rowSums()
# 
# extr_self %>%
#   dplyr::select(-c(sex, time, age)) %>%
#   (`<=`)(0) %>% 
#   colSums()
# 
# extr_chron %>%
#   dplyr::select(-c(sex, time, age)) %>%
#   (`<=`)(0) %>% 
#   colSums()
# 
# extr_gali %>%
#   dplyr::select(-c(sex, time, age)) %>%
#   (`<=`)(0) %>% 
#   colSums()
# 
# extr_adl %>%
#   dplyr::select(-c(sex, time, age)) %>%
#   (`<=`)(0) %>% 
#   colSums()
# 
# extr_iadl %>%
#   dplyr::select(-c(sex, time, age)) %>%
#   (`<=`)(0) %>% 
#   colSums()
# 
# # -----------------------------------------------------------------------------#
# self1 <- extr_self %>%
#   group_nest(sex, time) %>%
#   mutate(int_data = map(
#     data,
#     ~ .x %>%
#       interpolate_prob()
#   )) %>% 
#   dplyr::select(-data) %>% 
#   unnest(int_data)
# 
# self1 %>% 
#   filter(prob < 0)
# 
# chron1 %>% 
#   filter(prob < 0)
# 
# gali1 %>% 
#   filter(prob < 0)
# 
# # note these are always with transitions to death
# # maybe first adjust and then interpolate?
# # CHECK!
# adl1 %>% 
#   filter(prob < 0)
# 
# iadl1 %>% 
#   filter(prob < 0) %>% 
#   view()
# 
# 
# # 2 year transition probabilities from 20 to 110
# # 
# 
# age <- age - age %% 2
# group(sum)
# 
# first(l(x))
# sum(dx(x))
# 
# # 2 / bottom 1 
# dx  / l(x)
# 
# 2011
# 
# # 2011 hmd
# 
# 
# qx
# 
# 
# 
# z <- extr_gali %>% 
#   filter(sex == "female", time == 2011) %>% 
#   dplyr::select(-sex, -time) %>% 
#   filter(age %in% c(50:100))
# 
# U <- Ptibble2U_closed(z, interval = 2, start_age = 50)
# # 2) convert U to Q (rates) using U2Q()
# # U <- U[-nrow(U),-ncol(U)]
# Q <- U2Q(U, interval_current = 2)
# # 3) turn Q into a handy tibble using Q2Rtibble()
# Rtibble <- Q2Rtibble(Q, interval_current = 2)
# 
# 
# Rtibble$HD <- Rtibble$HD[1] + c(0, cumsum(abs(diff(Rtibble$HD))))
# 
# Rtibble$HH <- -(Rtibble$HD + Rtibble$HU)
# 
# 
# tail(Rtibble)
# 
# 
# 
# Rtibble %>% 
#   pivot_longer(-age,
#                names_to = "trns",
#                values_to = "val") %>% 
#   filter(trns %in% c("UD", "HD")) %>% 
#   ggplot(aes(x = age, y = val, color = trns)) + 
#   scale_y_log10()+
#   geom_line() + 
#   theme_minimal()
# 
# tail(Rtibble)
# 
# 
# # 4) now graduate the attrition
# Rtibble1 <- graduate_Rtibble(Rtibble, interval_current = 2, interval_desired = 1)
# # 5) take these single-age rates and convert back to Q using Rtibble2Q()
# Q1 <- Rtibble2Q(Rtibble1, interval = 1, start_age = 20)
# # 6) convert this back to U using Q2U()
# U1 <- Q2U(t(Q1), interval_desired = 1)
# # 7) convert U to a handy tibble using U2Ptibble()
# Ptibble1 <- U2Ptibble(U1, interval_current = 1)
# 
# install.packages("complexplus")
# 
# 
# self1 %>% 
#   
#   
#   
#   # eq 12.2 after interpolation but only for attrition
#   
#   
#   
#   extr_self %>% 
#   full_join(self_emp) %>%
#   filter(time == 2013) %>% 
#   ggplot() +
#   geom_line(aes(x     = age,
#                 y     = prob,
#                 group = to,
#                 color = to), linewidth = 1) +
#   geom_point(aes(x     = (age - age %% 2),
#                  y     = prob_emp,
#                  color = to)) +
#   facet_grid(from ~ sex, switch = "y") +
#   scale_y_continuous(breaks = pretty_breaks())+
#   scale_x_continuous(breaks = seq(20, 110, 5)) +
#   theme_light() +
#   theme(legend.position  = "bottom",
#         strip.placement  = "outside",
#         strip.background = element_blank(),
#         strip.text       = element_text(color = "black", face = "bold"),
#         axis.title.y     = element_blank(),
#         legend.title     = element_text(color = "black", face = "bold"))
# 
# 
# library(complexplus)
# 
# # # Compute the generator matrix Q
# # Q <- logm(U, method = "Higham08")
# # 
# # # Compute the 1-year transition probability matrix
# # P_half_year <- expm(Q * 0.5)
# 
# 
# 
# z$HU + z$HH + z$HD
# 
# z$UH 
# 
# 
# any(tet < 0)
# 
# 
# 
# 
# 
# # 4) now graduate the attrition
# Rtibble1 <- graduate_Rtibble(Rtibble, interval_current = 2, interval_desired = 1)
# # 5) take these single-age rates and convert back to Q using Rtibble2Q()
# Q1 <- Rtibble2Q(Rtibble1, interval = 1, start_age = 20)
# # 6) convert this back to U using Q2U()
# U1 <- Q2U(t(Q1), interval_desired = 1)
# # 7) convert U to a handy tibble using U2Ptibble()
# Ptibble1 <- U2Ptibble(U1, interval_current = 1)
# 
# 
# 
# 
# U <- Ptibble2U_closed(z, interval  = 2, start_age = 20)
# 
# # Compute the generator matrix Q
# Q <- logm(U, method = "Higham08")
# 
# # Compute the 1-year transition probability matrix
# P_half_year <- expm(Q * 0.5)
# 
# 
# P_half_year
# 
# 
# dimnames(P_half_year) <- dimnames(U)
# # here we are able to satisfy both sum and multiplication constraint
# # sum of all states = 1, and 1year * 1year = 2year
# 
# # calculate number of ages - dead data
# n_rows <- nrow(P_half_year) - 1
# # since we only have H-H and H-U, they can be pulled from U using the middle value
# middle <- n_rows / 2
# # these are H
# H1 <- 1:middle
# # there are U
# U1 <- (middle + 1):n_rows
# 
# # summary(as.vector(Q))      # Look for huge or complex values
# # summary(as.vector(P_half_year))
# # any(Im(P_half_year) != 0)
# # lets pull corresponding vectors
# H_H1 <-  diag(P_half_year)[H1]
# U_U1 <-  diag(P_half_year)[U1]
# H_D1 <-  P_half_year[nrow(P_half_year), H1]
# U_D1 <-  P_half_year[nrow(P_half_year), U1]
# H_U1 <-  1 - (H_H1 + H_D1) # for males
# U_H1 <-  1 - (U_U1 + U_D1)
# 
# # sum(H_H1 < 0)
# # sum(U_U1 < 0)
# # sum(H_D1 < 0)
# # sum(H_U1 < 0)
# # sum(U_H1 < 0)
# 
# final <- tibble(
#   H_H1 = H_H1,
#   H_H2 = H_H1,
#   H_U1 = H_U1,
#   H_U2 = H_U1,
#   H_D1 = H_D1,
#   H_D2 = H_D1,
#   U_U1 = U_U1,
#   U_U2 = U_U1,
#   U_H1 = U_H1,
#   U_H2 = U_H1,
#   U_D1 = U_D1,
#   U_D2 = U_D1
# ) %>%
#   mutate(ind = row_number(), age = seq(20, 110, 2)) %>%
#   pivot_longer(-c(ind, age), names_to  = "state", values_to = "p") %>%
#   dplyr::select(-ind) %>%
#   separate(state, c("from", "to")) %>%
#   # here we identify the interpolated age
#   mutate(age = ifelse(str_detect(to, pattern = "2$"), age + 1, age),
#          to = str_sub(to, 1, 1)) %>%
#   group_by(from, to) %>%
#   mutate(age1 = lead(age), p1   = lead(p)) %>%
#   # make the step function smooth, we fit line through the middle
#   mutate(age_mid = (age + age1) / 2, p_mid   = (p   + p1)   / 2) %>%
#   mutate(age_mid = floor(age_mid)) %>% 
#   dplyr::select(age = age_mid, prob = p_mid, from, to)
# 
# 
# 
# 
# chron1 <- extr_chron %>%
#   group_nest(sex, time) %>%
#   mutate(int_data = map(
#     data,
#     ~ .x %>%
#       interpolate_prob()
#   )) %>% 
#   dplyr::select(-data) %>% 
#   unnest(int_data)
# 
# gali1 <- extr_gali %>%
#   group_nest(sex, time) %>%
#   mutate(int_data = map(
#     data,
#     ~ .x %>%
#       interpolate_prob()
#   ))%>% 
#   dplyr::select(-data) %>% 
#   unnest(int_data)
# 
# adl1 <- extr_adl %>%
#   group_nest(sex, time) %>%
#   mutate(int_data = map(
#     data,
#     ~ .x %>%
#       interpolate_prob()
#   ))%>% 
#   dplyr::select(-data) %>% 
#   unnest(int_data)
# 
# iadl1 <- extr_iadl %>%
#   group_nest(sex, time) %>%
#   mutate(int_data = map(
#     data,
#     ~ .x %>%
#       interpolate_prob()
#   ))%>% 
#   dplyr::select(-data) %>% 
#   unnest(int_data)
# 
# 
# 
# save(extr_self,  file = "updated_results/extr_self.RData")
# save(extr_chron, file = "updated_results/extr_chron.RData")
# save(extr_gali,  file = "updated_results/extr_gali.RData")
# save(extr_adl,   file = "updated_results/extr_adl.RData")
# save(extr_iadl,  file = "updated_results/extr_iadl.RData")
# 
# 
# extr_self %>% 
#   full_join(self_emp) %>%
#   filter(time == 2013) %>% 
#   ggplot() +
#   geom_line(aes(x     = age,
#                 y     = prob,
#                 group = to,
#                 color = to), linewidth = 1) +
#   geom_point(aes(x     = (age - age %% 2),
#                  y     = prob_emp,
#                  color = to)) +
#   facet_grid(from ~ sex, switch = "y") +
#   scale_y_continuous(breaks = pretty_breaks())+
#   scale_x_continuous(breaks = seq(20, 110, 5)) +
#   theme_light() +
#   theme(legend.position  = "bottom",
#         strip.placement  = "outside",
#         strip.background = element_blank(),
#         strip.text       = element_text(color = "black", face = "bold"),
#         axis.title.y     = element_blank(),
#         legend.title     = element_text(color = "black", face = "bold"))
# 
# extr_chron %>% 
#   full_join(chron_emp) %>%
#   filter(time == 2013) %>% 
#   ggplot() +
#   geom_line(aes(x     = age,
#                 y     = prob,
#                 group = to,
#                 color = to), linewidth = 1) +
#   geom_point(aes(x     = (age - age %% 2),
#                  y     = prob_emp,
#                  color = to)) +
#   facet_grid(from ~ sex, switch = "y") +
#   scale_y_continuous(breaks = pretty_breaks())+
#   scale_x_continuous(breaks = seq(20, 110, 5)) +
#   theme_light() +
#   theme(legend.position  = "bottom",
#         strip.placement  = "outside",
#         strip.background = element_blank(),
#         strip.text       = element_text(color = "black", face = "bold"),
#         axis.title.y     = element_blank(),
#         legend.title     = element_text(color = "black", face = "bold"))
# 
# extr_gali %>% 
#   full_join(gali_emp) %>%
#   filter(time == 2013) %>% 
#   ggplot() +
#   geom_line(aes(x     = age,
#                 y     = prob,
#                 group = to,
#                 color = to), linewidth = 1) +
#   geom_point(aes(x     = (age - age %% 2),
#                  y     = prob_emp,
#                  color = to)) +
#   facet_grid(from ~ sex, switch = "y") +
#   scale_y_continuous(breaks = pretty_breaks())+
#   scale_x_continuous(breaks = seq(20, 110, 5)) +
#   theme_light() +
#   theme(legend.position  = "bottom",
#         strip.placement  = "outside",
#         strip.background = element_blank(),
#         strip.text       = element_text(color = "black", face = "bold"),
#         axis.title.y     = element_blank(),
#         legend.title     = element_text(color = "black", face = "bold"))
# 
# extr_adl %>% 
#   full_join(adl_emp) %>%
#   filter(time == 2013) %>% 
#   ggplot() +
#   geom_line(aes(x     = age,
#                 y     = prob,
#                 group = to,
#                 color = to), linewidth = 1) +
#   geom_point(aes(x     = (age - age %% 2),
#                  y     = prob_emp,
#                  color = to)) +
#   facet_grid(from ~ sex, switch = "y") +
#   scale_y_continuous(breaks = pretty_breaks())+
#   scale_x_continuous(breaks = seq(20, 110, 5)) +
#   theme_light() +
#   theme(legend.position  = "bottom",
#         strip.placement  = "outside",
#         strip.background = element_blank(),
#         strip.text       = element_text(color = "black", face = "bold"),
#         axis.title.y     = element_blank(),
#         legend.title     = element_text(color = "black", face = "bold"))
# 
# extr_iadl %>% 
#   full_join(iadl_emp) %>%
#   filter(time == 2011) %>% 
#   ggplot() +
#   geom_line(aes(x     = age,
#                 y     = prob,
#                 group = to,
#                 color = to), linewidth = 1) +
#   geom_point(aes(x     = (age - age %% 2),
#                  y     = prob_emp,
#                  color = to)) +
#   facet_grid(from ~ sex, switch = "y") +
#   scale_y_continuous(breaks = pretty_breaks())+
#   scale_x_continuous(breaks = seq(20, 110, 5)) +
#   theme_light() +
#   theme(legend.position  = "bottom",
#         strip.placement  = "outside",
#         strip.background = element_blank(),
#         strip.text       = element_text(color = "black", face = "bold"),
#         axis.title.y     = element_blank(),
#         legend.title     = element_text(color = "black", face = "bold"))
# 
# # -----------------------------------------------------------------------------#
# save(self1,  file = "updated_results/inter_self.RData")
# save(chron1, file = "updated_results/inter_chron.RData")
# save(gali1,  file = "updated_results/inter_gali.RData")
# save(adl1,   file = "updated_results/inter_adl.RData")
# save(iadl1,  file = "updated_results/inter_iadl.RData")
# # -----------------------------------------------------------------------------#