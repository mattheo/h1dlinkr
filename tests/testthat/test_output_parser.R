library(h1dlinkr)

context("Parse HYDRUS output")

path <- file.path("./data/SA_test/")
list.files(path, pattern = "\\.out")

t_level <- read_t_level(file.path(path, "T_level.out"))
a_level <- read_a_level(file.path(path, "A_level.out"))
nod_inf <- read_nod_inf(file.path(path, "Nod_Inf.out"))
# image(y = rev(attr(nod_inf, "depth")), z = apply(exp(log(nod_inf[["Head"]])), 1, rev), col = cm.colors(10))
# image(y = rev(attr(nod_inf, "depth")), z = apply((nod_inf[["Head"]]), 1, rev), col = (heat.colors(1000)), add = T)
# image(x = attr(nod_inf, "time"), y = rev(attr(nod_inf, "depth")), z = apply(nod_inf[["Flux"]], 1, rev), col = topo.colors(1000))
obs_node <- read_obs_node(file.path(path, "Obs_Node.out"))
balance <- read_balance(file.path(path, "Balance.out"))
run_stdout <- read_stdout(file.path(path, "run.out"))
output <- read_output(path)

# microbenchmark::microbenchmark(
#   output <- read_output(path)
# )
