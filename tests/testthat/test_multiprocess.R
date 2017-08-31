library(future)
library(doParallel)

run_par <- function(runs, template, threads) {
  plan(multiprocess, workers = threads)
  # threads <- length(cl)
  fs <- list()
  for(j in seq_len(runs)) {
    cat("create future", j, "\n")
    # print(system.time(
    fs[[j]] <- future(
      {
        current <- file.path("./tests/testthat/data/mp", formatC(j, width = 3, flag = 0))
        unlink(current, recursive = TRUE, force = TRUE)
        dir.create(current, recursive = TRUE)
        file.copy(template, current)
        h1dlinkr::run_h1d(current)
        # Sys.sleep(5)
        # Sys.getpid()
      }
    )
    # ))
    # the first few workers can be started without checking if they are finished
    if(j < threads) next
    repeat {
      # cat(system.time({x <- resolved(fs)})["elapsed"])
      cat(".")
      if (sum(!resolved(fs)) < threads) {
        cat("\nnext future\n")
        break
      }
      cat("-")
    }
  }
  values(fs)
}

run_serial <- function(runs, template) {
  out <- list()
  for(j in seq_len(runs)) {
    current <- file.path("./tests/testthat/data/mp", formatC(j, width = 3, flag = 0))
    unlink(current, recursive = TRUE, force = TRUE)
    dir.create(current, recursive = TRUE)
    file.copy(template, current)
    out[[j]] <- h1dlinkr::run_h1d(current)
  }
  unlink("./tests/testthat/data/mp", recursive = T, force = T)
  return(out)
}

run_foreach <- function(runs, template) {
  out <- foreach(j = icount(runs)) %dopar% {
    current <- file.path("./tests/testthat/data/mp", formatC(j, width = 3, flag = 0))
    unlink(current, recursive = TRUE, force = TRUE)
    dir.create(current, recursive = TRUE)
    file.copy(template, current)
    h1dlinkr::run_h1d(current)
  }
  unlink("./tests/testthat/data/mp", recursive = T, force = T)
  return(out)
}

proj_template <- dir("./tests/testthat/data/ga_l3_fit_100ET3L/", full.names = TRUE)
# serial <- system.time(a <- run_serial(10, proj_template))

parl <- system.time(b <- run_par(20, proj_template, threads = 4))

cl <- parallel::makeCluster(4)
registerDoParallel(cl)
doParallel <- system.time(c <- run_foreach(20, proj_template))
parallel::stopCluster(cl)
#
# print(serial)
# print(sapply(a, attr, which = "runtime"))
print(parl)
print(sum(sapply(b, attr, which = "runtime")))
# print(sapply(b, attr, which = "runtime"))
print(doParallel)
print(sum(sapply(c, attr, which = "runtime")))
#

unlink("./tests/testthat/data/mp", recursive = T, force = T)
