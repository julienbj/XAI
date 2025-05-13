library(mvtnorm)

generate_data <- function(N, betas, rho, incl_y=T){
  intercept <- betas[1]
  betas <- betas[-1]
  M <- length(betas)
  mu <- rep(0, M)
  
  if (rho!=0.0){
    Sigma <- matrix(rho, nrow=M, ncol=M)
    diag(Sigma) <- 1 
  } else{
    Sigma <- diag(M) 
  }
  
  X <- rmvnorm(n = N, sigma = Sigma)
  data <- as.data.frame(X)
  
  if (incl_y){
    y <- intercept + X %*% betas + rnorm(N)
    data$y <- y
  }
  return(data)
}
