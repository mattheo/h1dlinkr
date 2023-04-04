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

refine_grid <- function(grid, where, by) {
  section <- grid[grid >= where[1] & grid <= where[2]]
  refined <- diff(section) / by + head(section, -1)
  sort(c(refined, grid))
}
