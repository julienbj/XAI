source("functions/run_simulation.R")
source("functions/plot_results.R")

betas1 <- c(0.3, 0.1, 0.3, -0.5, 0.7)
betas2 <- c(3, 1, 3, -5, 7)

run_sim(betas=betas1, filename="betas1_runs50_N1000")

run_sim(betas=betas2, filename="betas2_runs50_N1000")
