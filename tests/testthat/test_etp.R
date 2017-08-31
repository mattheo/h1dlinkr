
etp <- c(0.05,0.04,0.07,0.08,0.07,0.02,0.03,0.03,0.03,0.02,0.03,0.04,0.05,0.01)
p <- c(0,0.56,0,0.16,1.99,1.21,0.87,1.91,0.08,0.15,0,0.35,0.53,0)
lai <- rep.int(1, length(etp))
e_t <- split_etp(etp, lai)
interception(p, e_t$evaporation, lai)
