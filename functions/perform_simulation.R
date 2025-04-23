source("functions/generate_data.R")
source("functions/compute_shap.R")

perform_simulation <- function(N=1000, M, dependence=F, include.shapr=T){
  train <- generate_data(N, M, dependence = dependence)
  test <- generate_data(500, M, dependence = dependence)
  
  X_test <- test$data[, 1:M]
  
  model = lm(y ~ ., data = train$data) 
  true_shap <- compute_shap(model=model, test_data = X_test, train=train, dependence=dependence)
  
  mae <- list()
  
  if (include.shapr) {
    library(shapr)
    
    explainer <- shapr(train$data, model)
    shapr_vals <- explain(X_test, explainer, approach = "empirical")$shapley_values
    shapr_vals <- shapr_vals[, colnames(X_test)]
    
    colnames(true_shap) <- colnames(shapr_vals)
    
    mae$shapr <- mean(abs(as.matrix(shapr_vals) - true_shap))
  }
  
  return(mae)
}

perform_simulation(M=5)
