# root distribution function in soil Hoffmann van Genuchten 1983
root_hoffmann <- function(L, Lr) {
  x <- -L:0
  y <- (2.0833 / Lr) * (1 - (L - abs(x)) / Lr)
  y[abs(x) > (L - 0.2 * Lr)] <- 1.667 / Lr
  y[abs(x) < (L - Lr)] <- 0
  y
}

# logistic model for root distribution
root_logistic <- function(n, location, scale) {
  seq(0, 1, length.out = n) %>%
    plogis(location = location/n, scale = scale/n, lower.tail = FALSE) %>%
    round(digits = 3)
}

root_gale <- function(n, beta) {
  y <- -beta^(seq.int(0, n)) * log(beta)
  round((y - min(y)) / (max(y) - min(y)), 3)
}
