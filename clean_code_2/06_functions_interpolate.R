# -----------------------------------------------------------------------------#
# creates a submatrix for a given transition,
# meant to be composed into U
pi2u <- function(pivec, 
                 from = "H",
                 to   = "H",
                 start_age = 50,
                 interval  = 1) {
  out           <- cbind(rbind(0, diag(pivec)), 0)
  n             <- length(pivec)
  ages          <- ((0:n) * interval) + start_age
  from_names    <- paste(from, ages, sep = "::")
  to_names      <- paste(to, ages, sep = "::")
  dimnames(out) <- list(to_names, from_names)
  out
}
# -----------------------------------------------------------------------------#
# composes U from a set of submatrices
u2U_closed <- function(HH, HU, UH, UU){
  out <- rbind(
    cbind(HH, UH),
    cbind(HU, UU))
  
  out <- cbind(rbind(out, 1 - colSums(out)),0)
  colnames(out)[ncol(out)] <- "D::Inf"
  rownames(out)[nrow(out)] <- "D::Inf"
  out[nrow(out),ncol(out)] <- 1
  out
}
# -----------------------------------------------------------------------------#
# avoid needing to make submatrices manually,
# takes care of upper left corner minor detail that
# caused the singularity error
Ptibble2U_closed <- function(Ptibble, 
                             interval  = 1, 
                             start_age = 50){
  n <- nrow(Ptibble) + 1
  HH <- Ptibble %>% 
    pull(HH) %>% 
    pi2u("H","H", start_age = start_age, interval = interval) %>% 
    '['(-1,-n) # hard coded for this example. 
  # Could be dealt with in pi2u() more generally
  HU <- Ptibble %>% 
    pull(HU) %>% 
    pi2u("H","U", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  UU <- Ptibble %>% 
    pull(UU) %>% 
    pi2u("U","U", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  UH <- Ptibble %>% 
    pull(UH) %>% 
    pi2u("U","H", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  
  U <- u2U_closed(HH, HU, UH, UU)
  U
}
# # -----------------------------------------------------------------------------#
interpolate_prob <- function(.data) {

  U <- Ptibble2U_closed(.data, interval  = 2, start_age = 20)

  # sum(colSums(U) != 1)
  # 
  # any(eigen(U)$values < 0)
  
  # Compute the generator matrix Q
  Q <- logm(U, method = "Higham08")

  # Compute the 1-year transition probability matrix
  P_half_year <- expm(Q * 0.5)

  # colSums(P_half_year)  # should all be ~1
  # range(P_half_year)    # should be [0,1]
  # any(P_half_year < 0)
  #

  # equal
  # (P_half_year %*% P_half_year)[2,2]
  # U[2,2]
  #
  # assign the dimnames
  dimnames(P_half_year) <- dimnames(U)
  # here we are able to satisfy both sum and multiplication constraint
  # sum of all states = 1, and 1year * 1year = 2year

  # calculate number of ages - dead data
  n_rows <- nrow(P_half_year) - 1
  # since we only have H-H and H-U, they can be pulled from U using the middle value
  middle <- n_rows / 2
  # these are H
  H1 <- 1:middle
  # there are U
  U1 <- (middle + 1):n_rows

  # summary(as.vector(Q))      # Look for huge or complex values
  # summary(as.vector(P_half_year))
  # any(Im(P_half_year) != 0)
  # lets pull corresponding vectors
  H_H1 <-  diag(P_half_year)[H1]
  U_U1 <-  diag(P_half_year)[U1]
  H_D1 <-  P_half_year[nrow(P_half_year), H1]
  U_D1 <-  P_half_year[nrow(P_half_year), U1]
  H_U1 <-  1 - (H_H1 + H_D1) # for males
  U_H1 <-  1 - (U_U1 + U_D1)

  # sum(H_H1 < 0)
  # sum(U_U1 < 0)
  # sum(H_D1 < 0)
  # sum(H_U1 < 0)
  # sum(U_H1 < 0)

  final <- tibble(
    H_H1 = H_H1,
    H_H2 = H_H1,
    H_U1 = H_U1,
    H_U2 = H_U1,
    H_D1 = H_D1,
    H_D2 = H_D1,
    U_U1 = U_U1,
    U_U2 = U_U1,
    U_H1 = U_H1,
    U_H2 = U_H1,
    U_D1 = U_D1,
    U_D2 = U_D1
  ) %>%
    mutate(ind = row_number(), age = seq(20, 110, 2)) %>%
    pivot_longer(-c(ind, age), names_to  = "state", values_to = "p") %>%
    dplyr::select(-ind) %>%
    separate(state, c("from", "to")) %>%
    # here we identify the interpolated age
    mutate(age = ifelse(str_detect(to, pattern = "2$"), age + 1, age),
           to = str_sub(to, 1, 1)) %>%
    group_by(from, to) %>%
    mutate(age1 = lead(age), p1   = lead(p)) %>%
    # make the step function smooth, we fit line through the middle
    mutate(age_mid = (age + age1) / 2, p_mid   = (p   + p1)   / 2) %>%
    # age in full years, in reality it is mid year
    mutate(age_mid = floor(age_mid)) %>%
    dplyr::select(age = age_mid, prob = p_mid, from, to)

  return(final)

}
# 

# # -----------------------------------------------------------------------------#
# # U is the standard transient matrix with probabilities,
# # in demgoraphers' orientation
U2Q <- function(U, interval_current = 1){
  Q <-
    U |>
    t() |>
    matlog() |>
    zapsmall() %>%
    # discount for interval!
    '/'(interval_current)
  dimnames(Q) <- U |> t() |> dimnames()
  Q
}
# 
# # -----------------------------------------------------------------------------#
# # extracts the rates and sticks them in a reasonable tibble
Q2Rtibble <- function(Q, interval_current = 2) {
  Q |>
    as.data.frame() |>
    rownames_to_column(var = "from") |>
    pivot_longer(-1, names_to = "to", values_to = "R") |>
    separate(to,
             into = c("state_to", "age_to"),
             sep = "::",
             convert = TRUE) %>%
    separate(from,
             into = c("state_from", "age_from"),
             sep = "::",
             convert = TRUE) |>
    filter(age_to == (age_from + interval_current) |
             is.infinite(age_to),
           !is.infinite(age_from)) |>
    select(-age_to) |>
    mutate(transition = paste0(state_from, state_to),
           .keep = "unused") |>
    pivot_wider(names_from = transition,
                values_from = R) |>
    rename(age = age_from)
}
# 
# # -----------------------------------------------------------------------------#
graduate_Rtibble <- function(Rtibble,
                             interval_current = 2,
                             interval_desired = 1
                             ){
  age_in  <- Rtibble$age
  age_out <- seq(age_in[1], max(age_in) + interval_current ,
                 by = interval_desired)
  age_fit <- age_in + interval_current / 2
  HD <- splinefun(age_fit, Rtibble$HD)(age_out)
  UD <- splinefun(age_fit, Rtibble$UD)(age_out)
  UH <- splinefun(age_fit, Rtibble$UH)(age_out)
  HU <- splinefun(age_fit, Rtibble$HU)(age_out)
  tibble(age_out, HD, HU, UD, UH) |>
    mutate(HH = -(HD + HU),
           UU = -(UD + UH))
}
# #  
# # -----------------------------------------------------------------------------#
Rtibble2Q <- function(Rtibble, interval = 1, start_age = 50){
  n <- nrow(Rtibble) + 1
  HH <- Rtibble |>
    pull("HH") |>
    pi2u(from = "H",
         to = "H",
         start_age = start_age,
         interval = interval) %>%
    '['(-1,-n)

  HU <- Rtibble |>
    pull(HU) |>
    pi2u("H","U",
         start_age = start_age,
         interval = interval) %>%
    '['(-1,-n)

  UU <- Rtibble |>
    pull(UU) |>
    pi2u("U","U",
         start_age = start_age,
         interval = interval) %>%
    '['(-1,-n)

  UH <- Rtibble |>
    pull(UH) |>
    pi2u("U","H",
         start_age = start_age,
         interval = interval) %>%
    '['(-1,-n)

  Q <- rbind(
    cbind(HH, UH),
    cbind(HU, UU))

  Q <- cbind(rbind(Q, -colSums(Q)),0)
  colnames(Q)[ncol(Q)] <- "D::Inf"
  rownames(Q)[nrow(Q)] <- "D::Inf"
  Q
}
# 
# # -----------------------------------------------------------------------------#
Q2U <- function(Q, interval_desired = 1){
  U <-
    Q %>%
    '*'(interval_desired) %>%
    expm::expm()
  dimnames(U) <- dimnames(Q)
  U |> t()
}
# 
# # -----------------------------------------------------------------------------#
# # extract the non-null probabilities from U of each possible transition
U2Ptibble <- function(U, interval_current = 1){

  U |>
    as.data.frame() |>
    rownames_to_column(var = "to") |>
    pivot_longer(-1, names_to = "from", values_to = "R") |>
    separate(to,
             into = c("state_to", "age_to"),
             sep = "::",
             convert = TRUE) %>%
    separate(from,
             into = c("state_from", "age_from"),
             sep = "::",
             convert = TRUE) |>
    filter(age_to == (age_from + interval_current) |
             is.infinite(age_to),
           !is.infinite(age_from)) |>
    select(-age_to) |>
    mutate(transition = paste0(state_from, state_to),
           .keep = "unused") |>
    pivot_wider(names_from = transition,
                values_from = R) |>
    rename(age = age_from)
}
# # -----------------------------------------------------------------------------#
do_grad <- function(.data) {
  
  U        <-  Ptibble2U_closed(.data, 
                                interval  = 2, 
                                start_age = 20)
  Q        <- U2Q(U, interval_current = 2)
  Rtibble  <- Q2Rtibble(Q, interval_current = 2)
  Rtibble1 <- graduate_Rtibble(Rtibble,
                               interval_current = 2,
                               interval_desired = 1)
  Q1       <- Rtibble2Q(Rtibble1,
                        interval  = 1, 
                        start_age = 20)
  U1       <- Q2U(t(Q1), interval_desired = 1)
  Ptibble1 <- U2Ptibble(U1, interval_current = 1)
  Ptibble1 <- Ptibble1 %>% 
    pivot_longer(-age,
                 names_to  = "trns",
                 values_to = "prob") %>% 
    mutate(from = str_sub(trns, 1, 1),
           to   = str_sub(trns, 2)) %>% 
    dplyr::select(-trns)
  
  return(Ptibble1)
}
# -----------------------------------------------------------------------------#