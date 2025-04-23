library(gtools)

compute_shap <- function(model,test_data, train, dependence=F){
  M <- ncol(test_data)
  N <- nrow(test_data)
  
  mu <- colMeans(train$data[, 1:M])
  beta <- train$beta
  intercept <- train$intercept
  
  if(dependence){
    v <- function(S, x){
      ...
    }
  } else{
    v <- function(S, x){
      if (length(S) == 0) {
        return(intercept + sum(beta * mu))
      }
      known_contrib <- sum(beta[S] * x[S])
      unknown_contrib <- sum(beta[-S] * mu[-S])
      
      return(intercept + known_contrib + unknown_contrib)
    }
  }

  phi <- function(i, x){
    phi_i <- 0
    others <- setdiff(1:M, i)

    for (k in 0:(M - 1)) {
      subsets_k <- combn(others, k, simplify = FALSE)

      for (S in subsets_k) {
        S_with_i <- sort(c(S, i))
        marginal_contribution <- v(S_with_i, x) - v(S, x)

        weight <- factorial(k) * factorial(M - k - 1) / factorial(M)

        phi_i <- phi_i + weight * marginal_contribution
      }
    }
    return(phi_i)
  }
  
  shap_matrix <- matrix(0, nrow = N, ncol = M)
  colnames(shap_matrix) <- colnames(test_data)
  
  for (n in 1:N) {
    x <- as.numeric(test_data[n, ])
    for (j in 1:M) {
      shap_matrix[n, j] <- phi(j, x)
    }
  }
  
  return(shap_matrix)
}
