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
  out <- file.path(project, "run.out")
  if (file.exists(out)) unlink(out, force = TRUE)
  if (async) {
    return(processx::process$new(h1d_exec, project, stdout = out, stderr = out))
  }
  runtime <- system.time(
    system2(h1d_exec, project, stdout = out, stderr = out)
  )
  run_stdout <- read_file(out)
  structure(
    read_output(project),
    path = project,
    success = str_detect(run_stdout, "Calculations have finished successfully."),
    runtime = as.double(runtime["elapsed"]),
    pid = Sys.getpid(),
    stdout = run_stdout
  )
}
