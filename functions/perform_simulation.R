source(generate_data)
source(compute_shap)

perform.simulation <- function(N=1000, M, dependence=F, include.shapr=T){
  train_data <- generate.data(N, M, dependence = dependence)
  test_data <- generate.data(500, M, dependence = dependence)
  
  # model = lm(train_data[2] ~ ., data = train_data[1]) 
  # prediction = model(data) 

  results <- compute.shap(model, data, include.shapr=include.shapr)
  
  return(results)
}
