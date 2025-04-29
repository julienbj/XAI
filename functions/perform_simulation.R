source("functions/generate_data.R")
source("functions/compute_shap.R")
source("functions/plot_results.R")

library(shapr)

perform_simulation <- function(runs=30, N=1000, M=5, rho=0.0, incl.independence=T, incl.empirical=T, incl.gaussian=T, incl.copula=T, incl.vaeac=T){
  methods <- c()
  if (incl.independence) methods <- c(methods, "independence")
  if (incl.empirical)    methods <- c(methods, "empirical")
  if (incl.gaussian)     methods <- c(methods, "gaussian")
  if (incl.copula)       methods <- c(methods, "copula")
  if (incl.vaeac)        methods <- c(methods, "vaeac")
  
  mae      <- setNames(vector("list", length(methods)), methods)
  time_sum <- setNames(as.list(rep(0, length(methods))), methods)
  
  for(i in 1:runs){
    cat("Starting run no.", i, "/", runs, ", rho = ", rho)
    train <- generate_data(N, M, rho = rho)
    test <- generate_data(500, M, rho = rho)
  
    X_train <- train$data[, grep("^V", names(train$data)), drop = FALSE]
    X_test <- test$data[, grep("^V", names(test$data)), drop = FALSE]
  
    model = lm(y ~ ., data = train$data)
    true_shap <- compute_shap(model=model, test_data = X_test, train=train, rho=rho)
    
    p0 <- mean(train$data$y)
    
    run_shapr <- function(method){
      base_args <- list(
        model     = model,
        x_explain = X_test,
        x_train   = X_train,
        approach  = method,
        phi0      = p0,
        verbose   = NULL
      )
      
      if (method == "vaeac") {
        base_args$seed          <- 1L   
      }
      
      explanation  <- do.call(explain, base_args)
      
      shapr_vals <- explanation$shapley_values_est
      shapr_matrix <- as.matrix(shapr_vals[, .(V1, V2, V3, V4, V5)])
      
      mae_q <- mean(abs(shapr_matrix - true_shap))
      
      return(mae_q)
    }
    
    for (method in methods) {
      print("inne")
      tm <- system.time(res <- run_shapr(method))
      print("runnet")
      mae[[method]]      <- c(mae[[method]], res)
      print("lagret")
      time_sum[[method]] <- time_sum[[method]] + unname(tm["elapsed"])
      cat("hei ", method, "\n")
    }
  }
  return(list(mae=mae, time_sum=time_sum))
}

# results <- list()
# for(rho_i in c(0.0, 0.3, 0.7, 0.9)){
#   cat("Running rho =", rho_i, "\n")
#   results[[length(results) + 1]] <- perform_simulation(runs=30, rho=rho_i)
# }
# 
# plot_results(results)


testtest <- perform_simulation(runs=1, rho=0.5)
