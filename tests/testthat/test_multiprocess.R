library(future)

runit <- function() {
  h1d_exec <- "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx/H1D_CALC.EXE"
  proj_template <- dir("../Hydrus-1D/Projects/nonlinear/nonlinear/", full.names = TRUE)

  futures <- list()
  for(j in 1:10) {
    futures[[j]] <- future({
      current <- file.path("./tests/testthat/data/nonlinear", formatC(j, width = 3, flag = 0))
      dir.create(current, recursive = TRUE)
      file.copy(proj_template, current)
      out <- run_h1d(h1d_exec, current)
    })
  }
  repeat {
    Sys.sleep(0.1)
    if(all(resolved(futures))) {
      print("done")
      break
    }
  }
  values(futures)
}

plan(multiprocess)
for (i in 1:10){
  print(system.time(outcome <- runit()))
}
# microbenchmark::microbenchmark(print(system.time(outcome <- runit())), times = 5L)

plan(sequential)
for (i in 1:10) {
  print(system.time(outcome <- runit()))
}
# microbenchmark::microbenchmark(print(system.time(outcome <- runit())), times = 5L)
