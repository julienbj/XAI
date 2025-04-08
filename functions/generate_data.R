library(mvtnorm)

generate.data <- function(N, M=5, dependence=F, rho=0.5){
  mu <- rep(0, M)
  
  if (dependence){
    Sigma <- matrix(rho, nrow=N, ncol=M)
    diag(Sigma) <- 1 
  } else{
    Sigma <- diag(M) 
  }
  
  X <- rmvnorm(n = N, sigma = Sigma)
  y <- 5 + X[,1] + 3*X[,2] + X[,3] + 2*X[,4] + 0.5*X[,5] + rnorm(N)
  
  data <- as.data.frame(X)
  data$y <- y
  
  return(data)
}

test <- generate.data(10, dependence=TRUE)
print(cor(test[[1]])) 
