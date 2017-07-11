library(future)

# spawn H1D from within R
run_h1d <- function(h1d_path, project_path) {
  outfile <- file.path(project_path, "run.out")
  file.create(outfile)
  h1d_run <- system2(h1d_path, shQuote(project_path), stdout = outfile)

  run_stdout <- readr::read_lines(outfile)

  list(
    code = h1d_run,
    stdout = run_stdout,
    converged = ifelse(
      grepl("Calculations have finished successfully.", tail(run_stdout, 1)),
      TRUE,
      FALSE),
    runtime = as.double(stringr::str_extract(tail(run_stdout, 2)[1], pattern = "\\d+.\\d+"))
  )
}


runit <- function() {
  plan(multiprocess)
  h1d_exec <- "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx/H1D_CALC.EXE"
  proj_template <- dir("../Hydrus-1D/Projects/nonlinear/nonlinear/", full.names = TRUE)

  futures <- listenv::listenv()
    x1 = future({
      current <- "./tests/testthat/data/nonlinear1/"
      dir.create(current)
      file.copy(proj_template, current)
      c(
        run_h1d(h1d_exec, current),
        pid =  Sys.getpid()
      )
      unlink(current)
    }),
    x2 = future({
      current <- "./tests/testthat/data/nonlinear2"
      dir.create(current)
      file.copy(proj_template, current)
      c(run_h1d(h1d_exec, current),
        pid =  Sys.getpid()
      )
    }),
    x3 = future({
      current <- "./tests/testthat/data/nonlinear3"
      dir.create(current)
      file.copy(proj_template, current, recursive = TRUE)
      c(run_h1d(h1d_exec, current),
        pid =  Sys.getpid()
      )
    }),
    x4 = future({
      current <- "./tests/testthat/data/nonlinear4"
      dir.create(current)
      file.copy(proj_template, current, recursive = TRUE)
      c(run_h1d(h1d_exec, current),
        pid =  Sys.getpid()
      )
    })
  )
  repeat {
    if(all(sapply(futures, future::resolved))) {
      print("done")
      break
    }
  }
  as.list(future::values(futures))
}

# print(system.time(outcome <- runit()))
