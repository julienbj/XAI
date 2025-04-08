source("generate_data.R")
source("compute_shap.R")

perform.simulation <- function(N=1000, M, dependence=F, include.shapr=T){
  train_data <- generate.data(N, M, dependence = dependence)
  test_data <- generate.data(500, M, dependence = dependence)
  
  model = lm(y ~ ., data = train_data) 
  predictions <- predict(model, newdata = test_data)
  
  true_shap <- true_shap(model=model, test_data = test_data, train_data=train_data)
  results <- estimate_shap(model=model, train_data, include.shapr=include.shapr, true_shap=true_shap)
  
  return(results)
}
