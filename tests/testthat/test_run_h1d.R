
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
print(out2)
print(attr(out2, "runtime"))
