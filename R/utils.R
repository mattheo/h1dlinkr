# extract the decimal part of a float
decimal <- function(x) {
  sign(x) * (x - floor(x))
}

# get hours, minutes and seconds from fractions
datetime_frac_days <- function(d) {
  seconds <- lubridate::seconds_to_period(86400 * d)
  lubridate::as_datetime(seconds)
}

copy_table <- function(obj, size = 4096, col.names = FALSE, row.names = FALSE, ...) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = row.names, col.names = col.names, ...)
  close(f)
}

split_along <- function(arr, n) {
  split(arr, arrayInd(seq_along(arr), dim(arr))[, n]) %>%
    lapply(array, dim = dim(arr)[-n], dimnames(arr)[-n]) %>%
    setNames(dimnames(arr)[[n]])
}

