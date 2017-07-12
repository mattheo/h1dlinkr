#' Execute H1D from within R
#'
#' @param h1d_exec
#' Path to H1D_CALC executable
#' @param project_path
#' Path to HYDRUS project folder
#' @import utils
#' @return list
#' @export
#'
run_h1d <- function(h1d_exec, project_path) {
  outfile <- file.path(project_path, "run.out")
  file.create(outfile)
  runtime <- system.time(
    errcode <- system2(h1d_exec, shQuote(project_path), stdout = outfile)
  )

  list(
    errcode = errcode,
    path = project_path,
    success = grepl("Calculations have finished successfully.",
                    tail(readr::read_lines(outfile), 1)),
    runtime = runtime["elapsed"],
    pid = Sys.getpid()
    # output = read_stdout(outfile)
  )
}
