library(mvtnorm)

generate_data <- function(N, M=5, beta=c(1, 3, 1, 2, 0.5), intercept=5, rho=0.0){
  mu <- rep(0, M)
  
  if (rho!=0.0){
    Sigma <- matrix(rho, nrow=M, ncol=M)
    diag(Sigma) <- 1 
  } else{
    Sigma <- diag(M) 
  }
  
  X <- rmvnorm(n = N, sigma = Sigma)
  y <- intercept + X %*% beta + rnorm(N)
  
  data <- as.data.frame(X)
  data$y <- y
  
  return(list(data=data, intercept=intercept, beta=beta))
}
