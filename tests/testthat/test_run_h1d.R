
project <- file.path("./data/SA_test")
system.time(out1 <- run_h1d(project))
print(attr(out1, "runtime"))

library(future)
plan(multiprocess)
system.time({
  f <- future(run_h1d(project))
  repeat {
    if (resolved(f)) {
      out2 <- value(f)
      break
    }
  }
})
print(attr(out2, "runtime"))

system.time({
  run <- run_h1d(project, async = TRUE)
  repeat {
    Sys.sleep(0.001)
    if (!run$is_alive()) {
      break
    }
  }
})
run$read_all_output()



