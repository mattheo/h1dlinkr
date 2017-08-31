
# function to calculate transit times by tracing water
transit <- function(v, z, time_stamp) {
  delta_t <- diff(time_stamp)
  range_t <- diff(range(time_stamp))
  delta_z <- diff(z)
  trace <- list()

  for (i in seq_along(delta_t)) {
    # z[1, i] <- 0 - delta_t[i] * v[1, i]
    trace[i] <- 0 - delta_t[i] * v[1, i]
    for (j in 1:ncol(v)) {
      if (j + i > ncol(v) | trace[[i]][j] > nrow(v)) {
        break
      }
      if(trace[[i]][j] <= 0) {
        trace[[i]][j] <- 0
        trace[[i]][(j + 1)] <- 1e-15
        next
      }
      dz <- delta_t[j + i] * v[ceiling(trace[[i]][j]), j + i]
      if (dz < delta_z) trace[[i]][(j + 1)] <- trace[[i]][j] - x
    }
  }
  return(list)
}
