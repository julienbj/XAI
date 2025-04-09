library(gtools)

true_shap <- function(model, test_data, train_data){
  M <- ncol(test_data)
  
  v <- function(S){
    ... #conditional expectations
  }
  
  phi <- function(i){
    phi_i <- 0
    others <- setdiff(test_data, test_data[i])
    
    # Må endre store deler av koden ift subset av indekser vs subset av selve dataen
    for (k in 1:M-1) {
      subsets_k <- combn(others, k, simplify = FALSE)
      
      for (S in subsets_k) {
        S_with_i <- sort(c(S, i))
        marginal_contribution <- v(S_with_i) - v(S)
        
        weight <- factorial(k) * factorial(M - k - 1) / factorial(M)
        
        phi_i <- phi_i + weight * marginal_contribution
      }
    }
  }
  
  phi <- list()
  
  for(i in 1:M){
    phi$i <- phi(i)
  }
  return(phi)
}
