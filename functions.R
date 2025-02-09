# creates a submatrix for a given transition,
# meant to be composed into U
pi2u <- function(pivec, from ="H",to = "H",start_age = 50,interval = 1) {
  out           <- cbind(rbind(0, diag(pivec)), 0)
  n             <- length(pivec)
  ages          <- ((0:n) * interval) + start_age
  from_names    <- paste(from, ages, sep = "::")
  to_names      <- paste(to, ages, sep = "::")
  dimnames(out) <- list(to_names, from_names)
  out
}

var_interval <- function(interval_desired = 1) {
  
  Rtibble1 <- graduate_Rtibble(Rtibble, interval_current = 1, interval_desired = interval_desired)
  Q1       <- Rtibble2Q(Rtibble1, interval = interval_desired, start_age = 17)
  U1       <- Q2U(t(Q1), interval_desired = interval_desired)
  Ptibble1 <- U2Ptibble(U1, interval_current = interval_desired)
  return(Ptibble1)  

}


Ptibble2U_closed <- function(Ptibble, 
                             interval = 1, 
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

# avoid needing to make submatrices manually,
# takes care of upper left corner minor detail that
# caused the singularity error
Ptibble2U_closed <- function(Ptibble, interval = 1, start_age = 50){
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

# U is the standard transient matrix with probabilities, 
# in demgoraphers' orientation
U2Q <- function(U, interval_current = 1){
  Q <-
    U |>
    t() |> 
    expm::logm() |>
    zapsmall() %>%
    # discount for interval!
    '/'(interval_current)
  dimnames(Q) <- U |> t() |> dimnames()
  Q
}
logm_small <- function(U, age=50, interval_current = 2){ 
  from <- paste(c("H","U","D"),c(age,age,"Inf"),sep="::")
  to   <- paste(c("H","U","D"),c(age+interval_current,age+interval_current,"Inf"),sep="::")
  U_small <- U[to,from] |> t()
  Q    <- logm(U_small,"Eigen")
  dimnames(Q) <- dimnames(U_small)
  Q
}


U2Q_hackish <- function(U, ages=seq(50,110,by=2), interval_current=2){
  Q <- t(U) * 0
  for (a in seq(50,100,by=interval_current)){
    Qi <-logm_small(U, age = a, interval_current = interval_current)
    Q[rownames(Qi),colnames(Qi)] <- Qi
  }
  Q <- Q / interval_current
  Q
}

# extracts the rates and sticks them in a reasonable tibble
Q2Rtibble <- function(Q, interval_current = 2){
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

graduate_Rtibble <- function(Rtibble, 
                             interval_current = 2, 
                             interval_desired = 1, 
                             method = "spline"){
  age_in  <- Rtibble$age
  age_out <- seq(age_in[1], max(age_in) + interval_current,
                 by = interval_desired) # monotonic spline for D
  age_fit <- age_in + interval_current / 2
  HD <- splinefun(age_fit, Rtibble$HD)(age_out)
  UD <- splinefun(age_fit, Rtibble$UD)(age_out)
  UH <- splinefun(age_fit, Rtibble$UH)(age_out)
  HU <- splinefun(age_fit, Rtibble$HU)(age_out)
  
  tibble(age_out, HD, HU, UD, UH) |> 
    mutate(HH = -(HD + HU),
           UU = -(UD + UH))
}


Rtibble2Q <- function(Rtibble, 
                      interval = 1, 
                      start_age = 50) {
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

# Q is the standard transient matrix, in Markov orientation.
Q2U <- function(Q, interval_desired = 1){
  U <- 
    Q1 %>% 
    '*'(interval_desired) %>% 
    expm::expm()  
  dimnames(U) <- dimnames(Q)
  U |> t()
}
extract_pi <- function(U, from = "H", to = "H"){
  
  # identify the part of the matrix of the from and to of interest
  from_cols   <- grepl(colnames(U), pattern = from)
  to_cols     <-  grepl(rownames(U), pattern = to) 
  U_block     <- U[to_cols, from_cols]
  # take the subdiagonal of the matrix 
  # without the last col (last transition it is always to the abs. one )
  diag(U_block[-1,-ncol(U)])
  
}

# extract the non-null probabilities from U of each possible transition
U2Ptibble <- function(U, interval_current=1){
  
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

complete_partial_Ptibble <- function(partial_Ptibble){
  partial_Ptibble <- as.data.frame(partial_Ptibble)
  
  all_from_to <- c("HH","HU","HD","UH","UU","UD")
  Missing <- setdiff(all_from_to, names(partial_Ptibble))
  
  if (length(Missing) == 0){
    return(partial_Ptibble)
  }
  partial_Ptibble[Missing] <- NA
  
  n <- nrow(partial_Ptibble)
  rownames(partial_Ptibble) <- 0:(n-1)
  
  partial_Ptibble %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column("age") %>% 
    pivot_longer(-age,names_to = "from_to", values_to = "p") %>% 
    mutate(from = substr(from_to, 1, 1)) %>% 
    group_by(from, age) %>% 
    mutate(p = if_else(is.na(p), 1 - sum(p, na.rm = TRUE), p)) %>% 
    ungroup() %>% 
    select(-from) %>% 
    pivot_wider(names_from = from_to, values_from = p) %>% 
    column_to_rownames(var="age")
}




make_plots <- function(.data) {
  
  age <- .data$age
  .data %>%
    dplyr::select(age, starts_with("H")) %>%
    dplyr::select("HU", "HD", "HH") %>%
    as.matrix() %>%
    alr() %>%
    as_tibble() %>%
    mutate(age = age) %>%
    pivot_longer(-age, names_to = "transition", values_to = "p") |>
    ggplot(aes(x = age, y = p, color = transition)) +
    geom_line() + 
    theme_minimal() + 
    ggtitle(str_c(diff(age)[1], " year interval from H"))+ 
    theme(legend.position = "bottom",
          axis.title.y = element_blank())
  
}


make_plots2 <- function(.data) {
  
  age  <- .data$age
  type <- .data$type
  .data %>%
    dplyr::select(age, starts_with("H")) %>%
    dplyr::select("HU", "HD", "HH") %>%
    as.matrix() %>%
    alr() %>%
    as_tibble() %>%
    mutate(age = age,
           type = type) %>%
    pivot_longer(-c(age, type), names_to = "transition", values_to = "p") |>
    ggplot(aes(x = age, y = p, color = transition, linetype = type)) +
    geom_line() + 
    theme_minimal() + 
    ggtitle("all intervals from H")+ 
    theme(legend.position = "bottom",
          axis.title.y = element_blank())
  
}



make_plotsU <- function(.data) {
  
  age <- .data$age
  .data %>%
    dplyr::select(age, starts_with("U")) %>%
    dplyr::select("UH", "UD", "UU") %>%
    as.matrix() %>%
    alr() %>%
    as_tibble() %>%
    mutate(age = age) %>%
    pivot_longer(-age, names_to = "transition", values_to = "p") |>
    ggplot(aes(x = age, y = p, color = transition)) +
    geom_line() + 
    theme_minimal() + 
    ggtitle(str_c(diff(age)[1], " year interval from U"))+ 
    theme(legend.position = "bottom",
          axis.title.y = element_blank())
  
}


make_plots2U <- function(.data) {
  
  age  <- .data$age
  type <- .data$type
  .data %>%
    dplyr::select(age, starts_with("U")) %>%
    dplyr::select("UH", "UD", "UU") %>%
    as.matrix() %>%
    alr() %>%
    as_tibble() %>%
    mutate(age = age,
           type = type) %>%
    pivot_longer(-c(age, type), names_to = "transition", values_to = "p") |>
    ggplot(aes(x = age, y = p, color = transition, linetype = type)) +
    geom_line() + 
    theme_minimal() + 
    ggtitle("all intervals from U")+ 
    theme(legend.position = "bottom",
          axis.title.y = element_blank())
  
}


make_plots_tim_H <- function(.data) {
  
  age  <- .data$age
  type <- .data$type
  
  .data %>% 
    dplyr::select(age, starts_with("H")) %>%
    dplyr::select("HU", "HD", "HH") %>%
    as.matrix() %>%
    alr() %>%
    as_tibble() %>%
    mutate(age = age,
           interval = type) %>%
    pivot_longer(-c(age, interval), names_to = "transition", values_to = "p") |>
    ggplot(aes(x = age, y = p, color = interval)) +
    facet_wrap(~transition)+
    geom_line() + 
    theme_minimal() + 
    theme(legend.position = "bottom",
          axis.title.y = element_blank())
  
}


make_plots_tim_U <- function(.data) {
  
  age <- .data$age
  type <- .data$type
  .data %>%
    dplyr::select(age, starts_with("U")) %>%
    dplyr::select("UH", "UD", "UU") %>%
    as.matrix() %>%
    alr() %>%
    as_tibble() %>%
    mutate(age = age,
           interval = type) %>%
    pivot_longer(-c(age, interval), names_to = "transition", values_to = "p") |>
    ggplot(aes(x = age, y = p, color = interval)) +
    facet_wrap(~transition)+
    geom_line() + 
    theme_minimal() + 
    theme(legend.position = "bottom",
          axis.title.y = element_blank())
  
} 





# calculate lh and lu
# ----------------------------------------------------------------- #
lets <- function(.data) {
  
  for(i in 1:length(.data$lu[-1])) {
    
    .data$lh[i + 1] <-
      .data$lh[i] * .data$`HH`[i] + .data$lu[i] * .data$`UH`[i]
    
    .data$lu[i + 1] <-
      .data$lu[i] * .data$`UU`[i] + .data$lh[i] * .data$`HU`[i]
    
  }
  
  return(.data)
  
}

# lu / lx is the prevalence of disability

# calculate lifetable
# ----------------------------------------------------------------- #
calculate_lt <- function(.data) {
  .data %>%
    mutate(lx = lu + lh,
           du = lu * `UD`,
           dh = lh * `HD`,
           dx = du + dh,
           qx = dx / lx,
           px = 1 - qx
    ) %>%
    mutate(Lh = (lh + lead(lh)) / 2,
           Lh = ifelse(is.na(Lh), (lh + (lh * `HH` + lu * `UH`)) / 2, Lh),
           Lu = (lu + lead(lu)) / 2,
           Lu = ifelse(is.na(Lu), (lu + (lu * `UU` + lh * `HU`)) / 2, Lu),
           Lx = Lh + Lu
    ) %>%
    mutate(mh = dh / Lh,
           mu = du / Lu,
           mx = dx / Lx) %>%
    mutate(Ra = mu / mh)
  
}

# recalculate probabilities to sum to one
# ----------------------------------------------------------------- #
recalculate <- function(x) {
  
  x[1:2] <- x[1:2] / sum(x[1:2]) * (1 - x[3])
  
  return(x)
  
}
