library(h1dlinkr)

context("Parse HYDRUS output")

path <- "./tests/testthat/data"

actual_t_level <- read_t_level(path)
actual_nod_inf <- read_nod_inf(path)
actual_obs_node <- read_obs_node(path)
