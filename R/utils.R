# extract the decimal part of a float
decimal <- function(x) {
  sign(x) * (x - floor(x))
}

# get hours, minutes and seconds from fractions
datetime_frac_days <- function(d) {
  seconds <- lubridate::seconds_to_period(86400 * d)
  lubridate::as_datetime(seconds)
}
