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
  if (async) {
    out <- file.path(project, "run.out")
    if (file.exists(out)) unlink(out, force = TRUE)
    return(processx::process$new(h1d_exec, project, stdout = out, stderr = out))
  } else {
    runtime <- system.time(
      p <- processx::run(h1d_exec, project, timeout = 30)
    )
  }
  structure(
    read_output(project),
    class = "h1d_output",
    path = project,
    success = stringr::str_detect(p$stdout, "Calculations have finished successfully."),
    runtime = as.double(runtime["elapsed"]),
    pid = Sys.getpid(),
    process = p
  )
}

#' @return processx object
#' @export
#' @rdname run_h1d
run_h1d_async <- function(project, h1d_exec = "h1d_calc") {
  project <- file.path(project)
  out <- file.path(project, "run.out")
  if (file.exists(out)) unlink(out, force = TRUE)
  processx::process$new(h1d_exec, project, stdout = out, stderr = out)

}
