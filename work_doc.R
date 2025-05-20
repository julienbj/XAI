source("functions/run_simulation.R")
source("functions/plot_results.R")

#Strong and weak signal betas
betas1 <- c(0.3, 0.1, 0.3, -0.5, 0.7, 0.2)
betas2 <- c(3, 1, 3, -5, 7, 2)

#Expansion on the betas for time-plot
betas <- c(0.3, 0.1, 0.3, -0.5, 0.7, 0.2, 0.7, 0.1, 0.3, 0.4, 0.2)

#Run main simulation (manually edited for each beta-set)
# run_sim(betas=betas1[1:4], filename="betas1_M3_runs50_N1000")
# run_sim(betas=betas2[1:4], filename="betas2_M3_runs50_N1000")

#Read saved results from main simulations
b1_m3 <- readRDS("results/betas1_M3_runs50_N1000")
b2_m3 <- readRDS("results/betas2_M3_runs50_N1000")

b1_m4 <- readRDS("results/betas1_M4_runs50_N1000")
b2_m4 <- readRDS("results/betas2_M4_runs50_N1000")

b1_m5 <- readRDS("results/betas1_M5_runs50_N1000")
b2_m5 <- readRDS("results/betas2_M5_runs50_N1000")

plot_mae(b2_m3, plottitle = "MAE for M = 3 (high-impact)", filename = "betas2_m3")

#Run time-simulations
# run_simulation(betas[1:7], rhos = c(0.5), filename="time_b2_m6")
# run_simulation(betas[1:8], rhos = c(0.5), filename="time_b2_m7")
# run_simulation(betas[1:9], rhos = c(0.5), filename="time_b2_m8")
# run_simulation(betas[1:10], rhos = c(0.5), filename="time_b2_m9")

#Read results from time-plot simulations
b2_m6 <- readRDS("results/time_b2_m6")
b2_m7 <- readRDS("results/time_b2_m7")
b2_m8 <- readRDS("results/time_b2_m8")
b2_m9 <- readRDS("results/time_b2_m9")

#Clean results for plotting
b2_m3 <- lapply(b2_m3, `[[`, "times")[[4]]
b2_m4 <- lapply(b2_m4, `[[`, "times")[[4]]
b2_m5 <- lapply(b2_m5, `[[`, "times")[[4]]

b2_m6 <- b2_m6[[1]]
b2_m7 <- b2_m7[[1]]
b2_m8 <- b2_m8[[1]]
b2_m9 <- b2_m9[[1]]

plot_times(b2_m3, b2_m4, b2_m5, b2_m6, b2_m7, b2_m8, b2_m9, filename = "timeplot_b2_rho4_m3-9_log")
