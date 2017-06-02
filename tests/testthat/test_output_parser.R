library(h1dlinkr)

context("Parse HYDRUS output")

path <- "./tests/testthat/data/ga_l3_fit_100ET3L"

actual_t_level <- read_t_level(path)
actual_nod <- read_nod_inf(path)
