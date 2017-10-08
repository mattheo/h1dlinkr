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
run_h1d <- function(project, h1d_exec = "h1d_calc") {
  project <- path.expand(project)
  run.out <- file.path(project, "RUN.OUT")
  # file.create(run.out)
  runtime <- system.time(
    system2(h1d_exec, project, stdout = run.out, stderr = run.out)
  )
  run <- read_lines(run.out)
  structure(
    run,
    success = any(str_detect(run, "Calculations have finished successfully.")),
    # warnings = warnings(),
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
  cat(sprintf("Project directory: %s \n", attr(x, "path")))
  if (attr(x, "success")) {
    cat(sprintf("Successful run finished in %.2f seconds. \n\n", attr(x, "runtime")))
  } else {
    cat(sprintf("Unsucessful run stopped after %.2f seconds.", attr(x, "runtime")), "\n")
  }
  # this is where the actual output begins
  begin <- grep("^\\s+Time", x)[1]
  writeLines(x[seq(from = begin, length.out = 6)])
  if (length(x[-(1:begin)]) > 13) cat("\n...\n\n")
  writeLines(tail(x, 7))
  # if(!is.null(attr(x, "warnings"))) print(attr(x, "warnings"))
}
