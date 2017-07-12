library(h1dlinkr)

context("Parse HYDRUS output")

path <- "./tests/testthat/data/ga_l3_fit_100ET3L/"

actual_t_level <- read_t_level(file.path(path, "T_level.out"))
actual_nod_inf <- read_nod_inf(file.path(path, "Nod_Inf.out"))
actual_obs_node <- read_obs_node(file.path(path, "Obs_Node.out"))
