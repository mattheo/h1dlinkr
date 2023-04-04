library(h1dlinkr)

context("Parse HYDRUS output")

project <- file.path("./data/SA_test/")
# list.files(project, pattern = "\\.out")

t_level <- read_t_level(file.path(project, "T_level.out"))
a_level <- read_a_level(file.path(project, "A_level.out"))
nod_inf <- read_nod_inf(file.path(project, "Nod_Inf.out"))
obs_node <- read_obs_node(file.path(project, "Obs_Node.out"))
balance <- read_balance(file.path(project, "Balance.out"))

output <- read_output(project)
print(output)

output_run <- read_output(run_h1d(project))
print(output_run)
# microbenchmark::microbenchmark(
#   output <- read_output(project)
# )

x <- read_output("data/morris_L100_DS50_2_run_00024/")

