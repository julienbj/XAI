library(mvtnorm)

generate.data <- function(N, M=5, dependence=F, rho=0.5){
  mu <- rep(0, M)
  
  if (dependence){
    Sigma <- matrix(rho, nrow=M, ncol=M)
    diag(Sigma) <- 1 
  } else{
    Sigma <- diag(M) 
  }
  
  X <- rmvnorm(n = N, sigma = Sigma)
  y <- 5 + X[,1] + 3*X[,2] + X[,3] + 2*X[,4] + 0.5*X[,5] + rnorm(N)
  return(list(X, y))
}

test <- generate.data(10, dependence=TRUE)
print(cor(test[[1]])) 
