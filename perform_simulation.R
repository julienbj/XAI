# To add: 
# - Option to compute Shapley-values post training and data generation? 
# - 

perform_simulation = function(N, p, dependence=F, include.shapr=T){
  data = generate_data(N, p, dependence=dependence)
  model = train.model(data)
  true.shap = compute.shap(model, data)
  results = array(NA, dim = c(2, p))
  if(include.shapr){
    res.shapr = nr.shapr()
    mae.shapr = mean(abs(res.shapr-true.shapr), )
    results[1, ] = mae.shapr
  }
  return(results)
}