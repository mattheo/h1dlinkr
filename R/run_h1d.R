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
    pid = Sys.getpid()
  )
}
