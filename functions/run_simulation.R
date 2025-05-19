source("functions/generate_data.R")
source("functions/compute_shap.R")

library(shapr)

#Function for running simulations of different rhos, saves to file if filename is given
run_simulation <- function(betas, rhos=c(0, 0.1, 0.3, 0.5, 0.7, 0.9), runs=50, N1=1000, N2=500, methods=c("independence", "empirical", "gaussian", "copula"), filename=""){
  result <- list()
  
  for(rho_i in rhos){
    cat("Running rho =", rho_i, "\n")
    result[[length(result) + 1]] <- perform_simulation(runs=runs, betas=betas, rho=rho_i, N1=N1, N2=N2, methods=methods)
  }
  
  if(nzchar(filename)){
    saveRDS(result, file = file.path("results", filename))    
    invisible(result)
  } else {
    return(result)
  }
}

#Function performing the actual simulations
perform_simulation <- function(runs, N1, N2, rho, betas, methods){
  mae      <- setNames(vector("list", length(methods)), methods)
  elapsed_times <- setNames(vector("list", length(methods)), methods)
  
  M <- length(betas)-1
  
  for(i in 1:runs){
    cat("Starting run no.", i, "/", runs, ", rho = ", rho, "\n")
    
    train <- generate_data(N1, betas=betas, rho = rho)
    X_train <- train[1:M]
    
    X_test <- generate_data(N2, betas=betas, rho = rho, incl_y=F)
  
    model = lm(y ~ ., data = train)
    true_shap <- compute_shap(model=model, test_data = X_test, train_data=X_train, rho=rho)
    
    p0 <- mean(train$y)
    
    run_shapr <- function(method){
      base_args <- list(
        model     = model,
        x_explain = X_test,
        x_train   = X_train,
        approach  = method,
        phi0      = p0,
        verbose   = NULL)
      
      if (method == "vaeac") {
        base_args$seed          <- 1L   
      }
      
      explanation  <- do.call(explain, base_args)
      shapr_vals <- explanation$shapley_values_est
      
      cols <- paste0("V", seq_len(M))
      shapr_matrix <- as.matrix(shapr_vals[, ..cols])
      
      mae_q <- mean(abs(shapr_matrix - true_shap))
      
      return(mae_q)
    }
    
    for (method in methods) {
      tm <- system.time(res <- run_shapr(method))  
      mae[[method]]      <- c(mae[[method]], res) 
      elapsed_times[[method]] <- c(elapsed_times[[method]], unname(tm["elapsed"]))
    }
    
  }
  return(list(mae=mae, times=elapsed_times))
}

