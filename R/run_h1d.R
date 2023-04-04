#' Execute HYDRUS 1D from within R
#'
#' @param project
#' Path to HYDRUS project folder
#' @param h1d_exec
#' Path to H1D_CALC executable
#' @param timeout
#' Time in seconds after which the run is terminated
#' @import utils
#' @return list
#'
#' @export
run_h1d <- function(project, h1d_exec = "h1d_calc", timeout) {
  UseMethod("run_h1d")
}


#' @rdname run_h1d
#' @export
run_h1d.default <- function(project = ".", h1d_exec = "h1d_calc", timeout = NULL) {
  project <- path.expand(project)
  run.out <- file.path(project, "RUN.OUT")
  # file.create(run.out)
  runtime <- system.time(
    exit <- if (is.numeric(timeout) && Sys.info()["sysname"] != "Windows") {
      system2("timeout", args = c(timeout, h1d_exec, project), stdout = run.out, stderr = run.out)
    } else {
      system2(h1d_exec, project, stdout = run.out, stderr = run.out)
    }
  )
  run <- c(
    read_lines(run.out, n_max = 50L),
    rep.int("       .", 3),
    "",
    tail(read_lines(run.out, skip = 50L), 50L)
  )
  out <- list(
    output = run,
    success = any(str_detect(run, "Calculations have finished successfully.")),
    # warnings = warnings(),
    project = project,
    name = str_trim(run[16]),
    runtime = as.double(runtime["elapsed"]),
    pid = Sys.getpid(),
    exit = exit
  )
  class(out) <- c("h1d_run")
  out
}

#' @rdname run_h1d
#' @export
run_h1d.h1d_project <- function(project, h1d_exec = "h1d_calc", timeout = NULL) {

}


#' Print h1d_run objects
#'
#' @param x
#' object of class h1d_run
#' @export
print.h1d_run <- function(h1d_run) {
  cat(sprintf("Project directory: %s \n", h1d_run$path))
  cat(sprintf("Project name     : %s \n", h1d_run$name))

  if(h1d_run$success) {
    cat(sprintf("Successful run finished in %.2f seconds. \n\n", h1d_run$runtime))
  } else {
    cat(sprintf("Unsucessful run stopped after %.2f seconds. \nExit Status: %s\n", h1d_run$runtime, h1d_run$exit))
    if (h1d_run$exit == 124) cat("Timeout! \n") else cat("\n")
  }
  # this is where the actual output begins
  begin <- grep("^\\s+Time", h1d_run$output)[1]
  if (is.na(begin)) {
    writeLines(h1d_run$output[-(1:14)])
  } else {
    writeLines(h1d_run$output[seq(from = begin, length.out = 10)])
    if (length(h1d_run$output[-(1:begin)]) > 13) cat("\n...\n\n")
    writeLines(tail(h1d_run$output, 10))
  }
}
