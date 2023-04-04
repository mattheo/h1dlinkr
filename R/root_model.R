# root distribution function in soil Hoffmann van Genuchten 1983
root_hoffmann <- function(L, Lr) {
  x <- -L:0
  y <- (2.0833 / Lr) * (1 - (L - abs(x)) / Lr)
  y[abs(x) > (L - 0.2 * Lr)] <- 1.667 / Lr
  y[abs(x) < (L - Lr)] <- 0
  y
}

root_logistic <- function(x, location = 10, scale = 5) {
  scaledx <- (x - min(x))/diff(range(x))
  round(plogis(sort(scaledx), location = location/length(x), scale = scale/length(x), lower.tail = FALSE), 3)
}

root_gale <- function(x, beta) {
  y <- -beta^sort(abs(x)) * log(beta)
  round((y - min(y)) / diff(range(y)), 3)
}

