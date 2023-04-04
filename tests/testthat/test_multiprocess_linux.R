#!/usr/bin/env Rscript
# read arguments poassed to the script

cat(
  "\n",
  "R Process started",
  "\n"
)

args = commandArgs(trailingOnly = TRUE)
project <- ifelse(is.null(args[1]), stop("Error: need project path"), path.expand(args[1]))
runs <- ifelse(is.null(args[2]), 50, as.integer(args[2]))

cat(
  "\n",
   sprintf("Starting multiprocess test on project %s with %i runs.", project, runs),
   "\n"
)

library(future)
library(doParallel)

run_future <- function(runs, options, folder, lazy = FALSE, sleep = 0.01) {
  fs <- list()
  for (j in seq_len(runs)) {
    # cat(paste("Starting future", j), "\n")
    fs[[j]] <-
      future(
        {
          # create new project folder
          path <- file.path(folder, sprintf("h1d_run_%05.f", id))
          # write HYDRUS input files to the directory
          h1dlinkr::write_input(path, options)
          # run HYDRUS
          run <- h1dlinkr::run_h1d(path)
          # Sys.sleep(stats::rnorm(1, mean = 5, sd = 1))
          # read output of run
          output <- h1dlinkr::read_output(run)
          saveRDS(output, file = paste0(path, ".rds"))
          rm(output)
          gc()
          run
        },
        lazy = lazy,
        globals = list(
          id = j,
          options = options,
          folder = folder
        )
      )
    if (lazy | j < nbrOfWorkers()) {
      next
    }
    while (sum(!resolved(fs)) >= nbrOfWorkers()) Sys.sleep(sleep)
  }
  resolve(fs)
}

run_foreach <- function(runs, options, folder) {
  foreach(id = icount(runs)) %dopar% {
    # create new project folder
    path <- file.path(folder, sprintf("h1d_run_%05.f", id))
    # write HYDRUS input files to the directory
    h1dlinkr::write_input(path, options)
    # run HYDRUS
    run <- h1dlinkr::run_h1d(path)
    # Sys.sleep(stats::rnorm(1, mean = 5, sd = 1))
    # read output of run
    output <- h1dlinkr::read_output(run)
    saveRDS(output, file = paste0(path, ".rds"))
    rm(output)
    gc()
    TRUE
  }
}

tmpfolder <- function(length = 20, pattern = "[a-zA-Z0-9]") {
  file.path(Sys.getenv("TMP"), stringi::stri_rand_strings(1, length = length, pattern))
}

tmp <- file.path(Sys.getenv("TMP"), "hydrus")
cat(tmp, sep = "\n")

threads <-
  ifelse(
    nchar(Sys.getenv("MOAB_PROCCOUNT")) == 0,
    parallel::detectCores() - 1,
    length(max(1, as.numeric(Sys.getenv("MOAB_PROCCOUNT")) * 2 - 1))
  )
print(cl <- parallel::makeForkCluster(threads))
on.exit(parallel::stopCluster(cl))


plan(cluster, workers = cl)
multi_future_async <- system.time(a <- run_future(runs, project, tmp, conjunct = FALSE))

# multi_future <- system.time(b <- run_future(runs, project, tmp, conjunct = TRUE))

registerDoParallel(cl)
multi_foreach <- system.time(c <- run_foreach(runs, project, tmp))

cat("\n",
  "Runtime multi_future_async",
  "\n")
print(multi_future_async)
cat(
  sprintf(
    "%i runs in %.2f seconds on %i threads - %f seconds per run",
    runs,
    multi_future_async["elapsed"],
    threads,
    multi_future_async["elapsed"]/runs
  ),
  sep = "\n"
)
a <- values(a)
cat(sprintf("Mean run time: %.2f", mean(sapply(a, attr, which = "runtime"))), sep = "\n")
# print(sapply(a, attr, which = "runtime"))
print(object.size(a), unit = "Mb")

# cat("\nAll futures at once\n")
# print(multi_future)
# b <- values(b)
# print(sum(sapply(b, attr, which = "runtime")))
# # print(sapply(b, attr, which = "runtime"))
# print(object.size(b), unit = "Mb")

# cat("\nforeach\n")
# print(multi_foreach)
# print(sum(sapply(c, attr, which = "runtime")))
# # print(sapply(c, attr, which = "runtime"))
# print(object.size(c), unit = "Mb")
