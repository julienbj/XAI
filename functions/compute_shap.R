
compute_shap <- function(model, test_data, train_data, rho=0.0){
  M <- ncol(test_data)
  N <- nrow(test_data)
  
  mu <- colMeans(train_data[, 1:M])
  betas <- coef(model)[names(coef(model)) != "(Intercept)"]
  intercept <- coef(model)["(Intercept)"]  
  
  if(rho!=0.0){
    v <- function(S, x) {
      if (length(S) == 0) {
        return(intercept + sum(betas * mu))
      }
      
      Sigma <- matrix(rho, M, M)
      diag(Sigma) <- 1                  
      
      if (length(S) < M) {
        Sigma_uk_k <- Sigma[-S, S,  drop = FALSE]
        Sigma_k_k  <- Sigma[S,   S,  drop = FALSE]
        
        mu_cond <- as.vector(
          mu[-S] + Sigma_uk_k %*% solve(Sigma_k_k) %*% (x[S] - mu[S])
        )
      } else {
        mu_cond <- numeric(0) 
      }
      
      known_contrib   <- sum(betas[S]   * x[S])
      unknown_contrib <- sum(betas[-S] * mu_cond)
      
      return(intercept + known_contrib + unknown_contrib)
    }
  } else{
    v <- function(S, x){
      if (length(S) == 0) {
        return(intercept + sum(betas * mu))
      }
      known_contrib <- sum(betas[S] * x[S])
      unknown_contrib <- sum(betas[-S] * mu[-S])
      
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
