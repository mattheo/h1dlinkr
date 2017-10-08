
project <- file.path("./data/ga_l3_fit_100ET3L/")
out1 <- run_h1d(project)
print(out1)
run_h1d("data/SA_test/")

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
