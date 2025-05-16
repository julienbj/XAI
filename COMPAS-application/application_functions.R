
run_shapr <- function(method, x_train, x_test, model, p0){
  explanation  <- explain(model     = model,
                          x_explain = x_test,
                          x_train   = x_train,
                          approach  = method,
                          phi0      = p0,
                          verbose   = NULL)
  
  shapr_vals <- explanation$shapley_values_est
  return(shapr_vals)
}
