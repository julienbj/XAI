
compute.shap <- function(model, data, include.shapr=T){
  true_shap <- compute.shap(model, data)

  mae <- list()
  
  if(include.shapr){
    res_shapr <- nr.shapr()
    mae$shapr <- mean(abs(res_shapr-true.shap), )
  }
  
  return(mae)
}
