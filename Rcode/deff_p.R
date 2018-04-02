# deff_p

deff_p <- function(N) {
  M <- seq_along(N)
  w <- N / sum(N)
  sum(w * M ^ 2) / sum(w * M) ^ 2
}

N_example <- c(25, 52, 15, 5, 2, 1)
sum(N_example * seq_along(N_example))

N_LV <- c(552310, 303668, 103073, 29391, 4653, 773, 172, 55, 15, 5, 5, 2)
sum(N_LV * seq_along(N_LV))

deff_p(N_example)
deff_p(N_LV)
