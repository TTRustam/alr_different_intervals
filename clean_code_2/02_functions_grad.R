# -----------------------------------------------------------------------------#
extrap_lm_down <- function(y) {
  
  x     <- seq(from = 50, to = 100, by = 2)
  xnew1 <- data.frame(x = seq(from = 20,  to = 48,  by = 2))
  xnew2 <- data.frame(x = seq(from = 102, to = 110, by = 2))
  y1   <-  predict(lm(y ~ x), newdata = xnew1)
  y2   <-  predict(lm(y ~ x), newdata = xnew2)
  c(y1, y, y2)

}

# -----------------------------------------------------------------------------#
extrapolate_type <- function(.data) {
  
  zz <- .data %>%
    unnest(qxdata) %>%
    set_names(str_remove_all(names(.), "-")) %>%
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

# -----------------------------------------------------------------------------#
# creates a submatrix for a given transition,
# meant to be composed into U
# pi2u <- function(pivec, 
#                  from = "H",
#                  to   = "H",
#                  start_age = 50,
#                  interval  = 1) {
#   out           <- cbind(rbind(0, diag(pivec)), 0)
#   n             <- length(pivec)
#   ages          <- ((0:n) * interval) + start_age
#   from_names    <- paste(from, ages, sep = "::")
#   to_names      <- paste(to, ages, sep = "::")
#   dimnames(out) <- list(to_names, from_names)
#   out
# }
# -----------------------------------------------------------------------------#
# composes U from a set of submatrices
# u2U_closed <- function(HH, HU, UH, UU){
#   out <- rbind(
#     cbind(HH, UH),
#     cbind(HU, UU))
# 
#   out <- cbind(rbind(out, 1 - colSums(out)),0)
#   colnames(out)[ncol(out)] <- "D::Inf"
#   rownames(out)[nrow(out)] <- "D::Inf"
#   out[nrow(out),ncol(out)] <- 1
#   out
# }
# -----------------------------------------------------------------------------#
# avoid needing to make submatrices manually,
# takes care of upper left corner minor detail that
# caused the singularity error
# Ptibble2U_closed <- function(Ptibble, 
#                              interval  = 1, 
#                              start_age = 50){
#   n <- nrow(Ptibble) + 1
#   HH <- Ptibble %>% 
#     pull(HH) %>% 
#     pi2u("H","H", start_age = start_age, interval = interval) %>% 
#     '['(-1,-n) # hard coded for this example. 
#   # Could be dealt with in pi2u() more generally
#   HU <- Ptibble %>% 
#     pull(HU) %>% 
#     pi2u("H","U", start_age = start_age, interval = interval) %>% 
#     '['(-1,-n)
#   UU <- Ptibble %>% 
#     pull(UU) %>% 
#     pi2u("U","U", start_age = start_age, interval = interval) %>% 
#     '['(-1,-n)
#   UH <- Ptibble %>% 
#     pull(UH) %>% 
#     pi2u("U","H", start_age = start_age, interval = interval) %>% 
#     '['(-1,-n)
#   
#   U <- u2U_closed(HH, HU, UH, UU)
#   U
# }
# # -----------------------------------------------------------------------------#
# interpolate_prob <- function(.data) {
#   
#   U <- Ptibble2U_closed(.data, interval  = 2, start_age = 50)
#   
#   if(any(eigen(U)$values < 0)) {
# 
#     U <- as.matrix(nearPD(U, eig.tol = 1e-6)$mat)
# 
#   }
#   
#   # Compute the generator matrix Q
#   Q <- logm(U, method = "Higham08")
# 
#   # Compute the 1-year transition probability matrix
#   P_half_year <- expm(Q * 0.5)
#   
#   # colSums(P_half_year)  # should all be ~1
#   # range(P_half_year)    # should be [0,1]
#   # any(P_half_year < 0)
#   #
#   
#   # equal
#   # (P_half_year %*% P_half_year)[2,2]
#   # U[2,2]
#   #
#   # assign the dimnames
#   dimnames(P_half_year) <- dimnames(U)
#   # here we are able to satisfy both sum and multiplication constraint
#   # sum of all states = 1, and 1year * 1year = 2year
#   
#   # calculate number of ages - dead data
#   n_rows <- nrow(P_half_year) - 1
#   # since we only have H-H and H-U, they can be pulled from U using the middle value
#   middle <- n_rows / 2
#   # these are H
#   H1 <- 1:middle
#   # there are U
#   U1 <- (middle + 1):n_rows
#   
#   # summary(as.vector(Q))      # Look for huge or complex values
#   # summary(as.vector(P_half_year))
#   # any(Im(P_half_year) != 0)
#   # lets pull corresponding vectors
#   H_H1 <-  diag(P_half_year)[H1]
#   U_U1 <-  diag(P_half_year)[U1]
#   H_D1 <-  P_half_year[nrow(P_half_year), H1]
#   U_D1 <-  P_half_year[nrow(P_half_year), U1]
#   H_U1 <-  1 - (H_H1 + H_D1) # for males
#   U_H1 <-  1 - (U_U1 + U_D1)
#   
#   # sum(H_H1 < 0)
#   # sum(U_U1 < 0)
#   # sum(H_D1 < 0)
#   # sum(H_U1 < 0)
#   # sum(U_H1 < 0)
# 
#   final <- tibble(
#     H_H1 = H_H1,
#     H_H2 = H_H1,
#     H_U1 = H_U1,
#     H_U2 = H_U1,
#     H_D1 = H_D1,
#     H_D2 = H_D1,
#     U_U1 = U_U1,
#     U_U2 = U_U1,
#     U_H1 = U_H1,
#     U_H2 = U_H1,
#     U_D1 = U_D1,
#     U_D2 = U_D1
#   ) %>%
#     mutate(ind = row_number(), age = seq(20, 110, 2)) %>%
#     pivot_longer(-c(ind, age), names_to  = "state", values_to = "p") %>%
#     dplyr::select(-ind) %>%
#     separate(state, c("from", "to")) %>%
#     # here we identify the interpolated age
#     mutate(age = ifelse(str_detect(to, pattern = "2$"), age + 1, age),
#            to = str_sub(to, 1, 1)) %>%
#     group_by(from, to) %>%
#     mutate(age1 = lead(age), p1   = lead(p)) %>%
#     # make the step function smooth, we fit line through the middle
#     mutate(age_mid = (age + age1) / 2, p_mid   = (p   + p1)   / 2) %>%
#     mutate(age_mid = floor(age_mid)) %>% 
#     dplyr::select(age = age_mid, prob = p_mid, from, to)
#   
#   return(final)
#   
# }
# 
# # -----------------------------------------------------------------------------#
# # U is the standard transient matrix with probabilities,
# # in demgoraphers' orientation
# U2Q <- function(U, interval_current = 1){
#   Q <-
#     U |>
#     t() |>
#     matlog() |>
#     zapsmall() %>%
#     # discount for interval!
#     '/'(interval_current)
#   dimnames(Q) <- U |> t() |> dimnames()
#   Q
# }
# 
# # -----------------------------------------------------------------------------#
# # extracts the rates and sticks them in a reasonable tibble
# Q2Rtibble <- function(Q, interval_current = 2) {
#   Q |>
#     as.data.frame() |>
#     rownames_to_column(var = "from") |>
#     pivot_longer(-1, names_to = "to", values_to = "R") |>
#     separate(to,
#              into = c("state_to", "age_to"),
#              sep = "::",
#              convert = TRUE) %>%
#     separate(from,
#              into = c("state_from", "age_from"),
#              sep = "::",
#              convert = TRUE) |>
#     filter(age_to == (age_from + interval_current) |
#              is.infinite(age_to),
#            !is.infinite(age_from)) |>
#     select(-age_to) |>
#     mutate(transition = paste0(state_from, state_to),
#            .keep = "unused") |>
#     pivot_wider(names_from = transition,
#                 values_from = R) |>
#     rename(age = age_from)
# }
# 
# # -----------------------------------------------------------------------------#
# graduate_Rtibble <- function(Rtibble,
#                              interval_current = 2,
#                              interval_desired = 1
#                              ){
#   age_in  <- Rtibble$age
#   age_out <- seq(age_in[1], max(age_in) + interval_current ,
#                  by = interval_desired)
#   age_fit <- age_in + interval_current / 2
#   HD <- splinefun(age_fit, Rtibble$HD)(age_out)
#   UD <- splinefun(age_fit, Rtibble$UD)(age_out)
#   UH <- splinefun(age_fit, Rtibble$UH)(age_out)
#   HU <- splinefun(age_fit, Rtibble$HU)(age_out)
#   tibble(age_out, HD, HU, UD, UH) |>
#     mutate(HH = -(HD + HU),
#            UU = -(UD + UH))
# }
# #  
# # -----------------------------------------------------------------------------#
# Rtibble2Q <- function(Rtibble, interval = 1, start_age = 50){
#   n <- nrow(Rtibble) + 1
#   HH <- Rtibble |>
#     pull("HH") |>
#     pi2u(from = "H",
#          to = "H",
#          start_age = start_age,
#          interval = interval) %>%
#     '['(-1,-n)
# 
#   HU <- Rtibble |>
#     pull(HU) |>
#     pi2u("H","U",
#          start_age = start_age,
#          interval = interval) %>%
#     '['(-1,-n)
# 
#   UU <- Rtibble |>
#     pull(UU) |>
#     pi2u("U","U",
#          start_age = start_age,
#          interval = interval) %>%
#     '['(-1,-n)
# 
#   UH <- Rtibble |>
#     pull(UH) |>
#     pi2u("U","H",
#          start_age = start_age,
#          interval = interval) %>%
#     '['(-1,-n)
# 
#   Q <- rbind(
#     cbind(HH, UH),
#     cbind(HU, UU))
# 
#   Q <- cbind(rbind(Q, -colSums(Q)),0)
#   colnames(Q)[ncol(Q)] <- "D::Inf"
#   rownames(Q)[nrow(Q)] <- "D::Inf"
#   Q
# }
# 
# # -----------------------------------------------------------------------------#
# Q2U <- function(Q, interval_desired = 1){
#   U <-
#     Q %>%
#     '*'(interval_desired) %>%
#     expm::expm()
#   dimnames(U) <- dimnames(Q)
#   U |> t()
# }
# 
# # -----------------------------------------------------------------------------#
# # extract the non-null probabilities from U of each possible transition
# U2Ptibble <- function(U, interval_current = 1){
# 
#   U |>
#     as.data.frame() |>
#     rownames_to_column(var = "to") |>
#     pivot_longer(-1, names_to = "from", values_to = "R") |>
#     separate(to,
#              into = c("state_to", "age_to"),
#              sep = "::",
#              convert = TRUE) %>%
#     separate(from,
#              into = c("state_from", "age_from"),
#              sep = "::",
#              convert = TRUE) |>
#     filter(age_to == (age_from + interval_current) |
#              is.infinite(age_to),
#            !is.infinite(age_from)) |>
#     select(-age_to) |>
#     mutate(transition = paste0(state_from, state_to),
#            .keep = "unused") |>
#     pivot_wider(names_from = transition,
#                 values_from = R) |>
#     rename(age = age_from)
# }
# # -----------------------------------------------------------------------------#
# do_grad <- function(.data) {
# 
#   .data %>%
#     unnest(qxdata) %>%
#     filter(age < 86) %>%
#     group_nest(sex, time) %>%
#     mutate(data = map(data, ~ .x %>%
#                         set_names(
#                           c("age", "HH", "HU", "HD", "UH", "UU", "UD")
#                         ))) %>%
#     # Ptibble2U interval = 2
#     mutate(U = map(data, ~ Ptibble2U_closed(.x,
#                                             interval  = 2,
#                                             start_age = 50
#     ))) %>%
#     # U2Q interval = 2
#     mutate(Q = map(U, ~ U2Q(.x, interval_current = 2))) %>% #!!!!!!!!!
#     # Q2Rtibble interval = 2
#     mutate(Rtibble = map(Q, ~ Q2Rtibble(.x, interval_current = 2))) %>%
#     # graduate_Rtibble from 2 to 1
#     mutate(Rtibble1 = map(Rtibble, ~ graduate_Rtibble(.x,
#         interval_current = 2,
#         interval_desired = 1
#       )
#     )) %>%
#     # Rtibble2Q interval = 1
#     mutate(Q1 = map(Rtibble1, ~ Rtibble2Q(.x,
#                                           interval  = 1,
#                                           start_age = 50
#     ))) %>%
#     # Q2U interval = 1
#     mutate(U1 = map(Q1, ~ Q2U(t(.x),
#                               interval_desired = 1))) %>%
#     # U2Ptibble interval = 1
#     mutate(Ptibble1 = map(U1, ~ U2Ptibble(.x, interval_current = 1))) %>%
#     dplyr::select(sex, time, Ptibble1) %>%
#     unnest(Ptibble1)
# }
# -----------------------------------------------------------------------------#
# extrap_lm_down <- function(y) {
#   x    <- 50:112
#   xnew <- data.frame(x = 19:49)
#   y2   <-  predict(lm(y ~ x), newdata = xnew)
#   c(y2, y)
# }

# extrap_lm_down <- function(y) {
#   # x     <- 50:90
#   # # xnew1 <- data.frame(x = 19:49)
#   # xnew2 <- data.frame(x = (max(x) + 1):110)
#   # # y1    <-  predict(lm(y ~ x), newdata = xnew1)
#   # y2    <-  predict(lm(y ~ x), newdata = xnew2)
#   # c(y, y2)
#   x        <- 50:90
#   x_new    <- 91:110
#   # Fit linear model only to the last few points (e.g. last 5)
#   lm_fit   <- lm(y ~ x, subset = (x >= 85))
#   slope    <- coef(lm_fit)[["x"]]
#   last_val <- tail(y, 1)
#   y2       <- last_val + slope * (x_new - 85)
#   c(y, y2)
# }
# 
# extrap_lm_down <- function(y) {
#   x        <- 50:90           # Original age range
#   x_new    <- 91:110          # Target ages for extrapolation
#   
#   # Fit a linear model only on the last few points (e.g., age 85 to 90)
#   lm_fit   <- lm(y ~ x, subset = (x >= 80))
#   
#   
#   
#   y2 <- predict(lm_fit, data.frame(x = x_new))
#   
#   # # Extract the slope from the model
#   # slope    <- coef(lm_fit)[["x"]]
#   # 
#   # # Get the last known value from y (at age 90)
#   # last_val <- tail(y, 1)
#   # 
#   # # Extrapolate by extending the line from age 90 using the slope
#   # y2       <- last_val + slope * (x_new - 90)
#   # 
#   # # Combine original y with extrapolated y2
#   c(y, y2)
# }


# -----------------------------------------------------------------------------#
# extrapolate_type <- function(.data) {
# 
#   zz <- .data %>%
#     group_nest(sex, time) %>% # test time, test age
#     mutate(
#       hh_alr = map(
#         data,
#         ~ .x %>%
#           dplyr::select(age, starts_with("H")) %>%
#           dplyr::select("HU", "HD", "HH") %>%
#           as.matrix() %>%
#           alr() %>%
#           apply(2, extrap_lm_down)
#       )
#     ) %>%
#     mutate(
#       nh_alr = map(
#         data,
#         ~ .x %>%
#           dplyr::select(age, starts_with("U")) %>%
#           dplyr::select("UH", "UD", "UU") %>%
#           as.matrix() %>%
#           alr() %>%
#           apply(2, extrap_lm_down)
#       )
#     ) %>%
#     mutate(hh_inverse = map(
#       hh_alr,
#       ~ .x %>%
#         alrInv()  %>%
#         as.data.frame() %>%
#         rename(`HH` = V3)
#     )) %>%
#     mutate(nh_inverse = map(
#       nh_alr,
#       ~ .x %>%
#         alrInv()  %>%
#         as.data.frame() %>%
#         rename(`UU` = V3)
#     ))
# 
#   return(zz)
# 
# }
# -----------------------------------------------------------------------------#