library(future)
library(doParallel)

run_future <- function(runs, options, folder, conjunct = FALSE) {
  fs <- list()
  for(j in seq_len(runs)) {
    cat(paste("\nStarting future", j), "\n")
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
          h1dlinkr::read_output(run)
        },
        globals = list(id = j,
                       options = options,
                       folder = folder)
      )
    if(conjunct | j < nbrOfWorkers()) {
      next
    }
    repeat {
      print(system.time(
        x <- resolved(fs)
      ))
      if (sum(!x) < nbrOfWorkers()) {
        cat(paste(sum(!x), "unresolved futures"), "\n")
        break
      }
    }
  }
  fs
}

# h1d_future <- function(id, options, folder, ...) {
#   future(
#     {
#       # create new project folder
#       path <- file.path(folder, sprintf("h1d_run_%05.f", id))
#       # write HYDRUS input files to the directory
#       h1dlinkr::write_input(path, options)
#       # run HYDRUS
#       run <- h1dlinkr::run_h1d(path)
#       # Sys.sleep(stats::rnorm(1, mean = 5, sd = 1))
#       # read output
#       h1dlinkr::read_output(run)
#     },
#     globals = c("id", "options", "folder"),
#     ...
#   )
# }
#
# run_serial <- function(runs, template) {
#   out <- list()
#   for(j in seq_len(runs)) {
#     current <- file.path("./data/mp", formatC(j, width = 3, flag = 0))
#     unlink(current, recursive = TRUE, force = TRUE)
#     dir.create(current, recursive = TRUE)
#     file.copy(template, current)
#     out[[j]] <- h1dlinkr::run_h1d(current)
#   }
#   unlink("./data/mp", recursive = T, force = T)
#   return(out)
# }

run_foreach <- function(runs, options, folder) {
  foreach(j = icount(runs)) %dopar% {
    # create new project folder
    path <- file.path(folder, sprintf("h1d_run_%05.f", id))
    # write HYDRUS input files to the directory
    h1dlinkr::write_input(path, options)
    # run HYDRUS
    run <- h1dlinkr::run_h1d(path)
    # Sys.sleep(stats::rnorm(1, mean = 5, sd = 1))
    # read output of run
    h1dlinkr::read_output(run)
  }
}

tmpfolder <- function(length = 20, pattern = "[a-zA-Z0-9]") {
  file.path(Sys.getenv("TMP"), stringi::stri_rand_strings(1, length = length, pattern))
}


cl <- parallel::makeForkCluster()
project <- "~/Studium/Master Thesis/h1dlinkr/data/SA_test"

plan(cluster, workers = cl)
multi_future <- system.time(b <- run_future(20, project, tmpfolder(), conjunct = TRUE))
b <- values(b)

registerDoParallel(cl)
doParallel <- system.time(c <- run_foreach(20, project, tmpfolder()))

print(multi_future)
print(sum(sapply(b, attr, which = "runtime")))
print(sapply(b, attr, which = "runtime"))

print(doParallel)
print(sum(sapply(c, attr, which = "runtime")))
print(sapply(c, attr, which = "runtime"))

parallel::stopCluster(cl)
