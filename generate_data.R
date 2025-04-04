library(mvtnorm)

generate_data = function(N, p=5, dependence=F, rho=0.5){
  mu = rep(0, p)
  
  if (dependence){
    Sigma = matrix(rho, nrow=p, ncol=p)
    diag(Sigma) = 1 
  } else{
    Sigma = diag(p) 
  }
  
  X = rmvnorm(n = N, sigma = Sigma)
  y = 5 + X[,1] + 3*X[,2] + X[,3] + 2*X[,4] + 0.5*X[,5] + rnorm(N)
  return(list(X, y))
}

test = generate_data(10, dependence=TRUE)
print(cor(test[[1]])) 
