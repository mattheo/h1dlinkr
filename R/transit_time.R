
# function to calculate transit times by tracing water
transit <- function(q, theta, z, time) {
  delta_t <- diff(time)
  range_t <- diff(range(time))
  delta_z <- diff(z)
  trace <- list()

  v <- as.matrix(q) / as.matrix(theta)

  for (i in seq_along(delta_t)) {
    # z[1, i] <- 0 - delta_t[i] * q[1, i]
    trace[[i]] <- delta_t[i] * v[1, i]
    for (j in 1:ncol(v)) {
      # if (j + i > ncol(v) | trace[[i]][j] < min(z)) {
      # if at the current step we are below the column depth, stop the current trace
      if (trace[[i]][j] < min(z)) {
        break
      }
      # if at the current step we are above the soil surface, set the next step
      # to slightly below the surface and continue from there
      if(trace[[i]][j] >= 0) {
        trace[[i]][j] <- 0
        trace[[i]][(j + 1)] <- -1e-15
        next
      }
      # calculate the distance the water particle moves during the current time step

      z_level <- findInterval(abs(trace[[i]][j]), abs(z))
      dz <- delta_t[j + i] * v[z_level, j + i]
      if (dz < delta_z) trace[[i]][(j + 1)] <- trace[[i]][j] - z
    }
  }
  return(trace)
}
