#' Execute H1D from within R
#'
#' @param project
#' Path to HYDRUS project folder
#' @param h1d_exec
#' Path to H1D_CALC executable
#' @import utils
#' @return list
#' @export
#'
run_h1d <- function(project, h1d_exec = "h1d_calc", async = FALSE) {
  project <- file.path(project)
  run.out <- file.path(project, "run.out")
  if (async) {
    return(processx::process$new(h1d_exec, project, stdout = "|", stderr = "|"))
  }
  runtime <- system.time(
    system2(h1d_exec, project, stdout = run.out, stderr = run.out)
  )
  run <- read_lines(run.out)
  structure(
    run,
    success = str_detect(tail(run, 1), "Calculations have finished successfully."),
    path = project,
    runtime = as.double(runtime["elapsed"]),
    pid = Sys.getpid(),
    class = c("h1d_run")
  )
}


#' Pretty print HYDRUS runs
#'
#' @param x
#' object of class h1d_run
#' @export
#'
print.h1d_run <- function(x) {
  if (attr(x, "success")) {
    cat(sprintf("Successful run finished in %.2f seconds.", attr(x, "runtime")), "\n")
  } else {
    cat(sprintf("Unsucessful run stopped after %f seconds."), "\n")
  }
  cat(sprintf("Project directory: %s", attr(x, "path")), "\n")
  writeLines(x[28:40])
  cat("...\n")
}
