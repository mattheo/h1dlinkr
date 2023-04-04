
project <- file.path("data/SA_test/")
run1 <- run_h1d(project)
print(run1)

library(future)
plan(multiprocess)
system.time({
  f <- future(run_h1d(project))
  repeat {
    if (resolved(f)) {
      run2 <- value(f)
      break
    }
  }
})
print(run2)
print(run2$runtime)
