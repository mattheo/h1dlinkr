
project <- "./data/ga_l3_fit_100ET3L/"
system.time(out <- run_h1d(project))
print(attr(out, "runtime"))

library(future)
plan(multiprocess)
system.time({
  f <- future(run_h1d(project))
  repeat {
    if (resolved(f)) {
      out <- value(f)
      break
    }
  }
})
print(attr(out, "runtime"))

system.time({
  run <- run_h1d_async(project)
  repeat {
    Sys.sleep(0.001)
    if (run$is_alive() == FALSE) {
      break
    }
  }
})



