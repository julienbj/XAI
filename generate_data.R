library(mvtnorm)

generate_data = function(N, p=5, dependence=F){
  mu = rep(0, p)
  Sigma = diag(p)
  
  X = rmvnorm(n = N, sigma = Sigma)
  y = 5 + X[,1] + 3*X[,2] + X[,3] + 2*X[,4] + 0.5*X[,5] + rnorm(N)
  return(list(X, y))
}

test = generate_data(10)
print(dim(test[1])) 
