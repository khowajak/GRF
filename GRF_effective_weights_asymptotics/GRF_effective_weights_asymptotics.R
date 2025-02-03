rm(list=ls())
library(grf)
library(ggplot2)
library(glue)
set.seed(42)


get_x = function(n, c){
  X = matrix(sort(runif(n,-c, c)), nrow = n)
  colnames(X) = 'X1'
  return(X)
}
get_x_grid = function(n, c){
  X = matrix(seq(-c, c, length.out = n), nrow = n)
  colnames(X) = 'X1'
  return(X)
}
polynomial = function(x, p){
  X = matrix(1, nrow = nrow(x), ncol = ncol(x) * p + 1)
  cols = c('const')
  for (j in 1:ncol(x)){
    for (i in 1:p) {
      X[, (j - 1) * ncol(x) + i + 1] = x[, j] ** i
      cols = c(cols, glue("{colnames(x)[j]}^{i}"))
    }
  }
  colnames(X) = cols
  return(X)
}
theta_polynomial = function(x, p, beta){
  return(polynomial(x, p) %*% beta)
}

get_y = function(X, theta, sigma, seed=NULL){
  set.seed(NULL)
  n = nrow(X)
  return(theta(X) + rnorm(n, 0, sigma))
}
V_func = function(x, mu, sig, clip = NULL){
  res = -dnorm(x, mean = mu, sd = sig)
  if (!is.null(clip))
    res = sign(res) * pmax(abs(res), clip)
  return(res)
}
eps_tilde = function(X, Y, theta, sig, tau, clipV=NULL){
  res = matrix(0, nrow = nrow(X), ncol = length(tau))
  V = - V_func(theta(X), theta(X), sig, clip=clipV) ^ (-1)
  I = Y <= theta(X)
  for (i in 1:length(tau)){
    res[, i] = V * (tau[i] - I)
  }
  return(res)
}

tau = c(0.5)
sig = 1
width = 0.2
beta = c(0, 1, 0, 4)
p = 3
c = 1


rfs = list()
reps = 100
rmse_results = list()

for (n in c(500,1000,2000)){
  # Store sup-norm results for current n
  sup_diffs = numeric(reps)
  set.seed(123)
  for (rep in 1:reps) {  
  X = get_x(n, c)
  theta = function(X) theta_polynomial(X, p, beta)
  Y = get_y(X, theta, sig, 42)
  rfs[[as.character(n)]] = rf = grf::quantile_forest(X, Y, quantiles = tau)
  alpha = get_forest_weights(rf)
  theta_tilde = theta(X) - alpha %*% eps_tilde(X, Y, theta, sig, tau)
  theta_hat = predict(rf,X)$predictions
  # Compute sup-norm of the absolute difference
  sup_diffs[rep] = max(abs(theta_hat - theta_tilde))
  }
  
  # Compute RMSE comparing sup-norms to true theta
  true_theta_values = theta(X)
  rmse = sqrt(mean((sup_diffs - max(abs(true_theta_values)))^2)) #is that correct?
  rmse_results[[as.character(n)]] = rmse

  fn = glue(
    'qRF_polynom___beta{paste(beta, collapse="_")}___',
    'theta_tilde___',
    'q{formatC(tau*100, width=3, flag="0")}___',
    'sigma{formatC(sig*100, width=3, flag="0")}___',
    'n{formatC(as.integer(n), width=4, flag="0")}',
    '.png'
  )
  
  png(file = fn, res = 100)
  plot(X[, 'X1'], Y, type = 'p', pch = 20, cex = 0.09, xlab='X1')
  lines(X[, 'X1'], theta_tilde, col = 'red', lwd = 2)
  lines(X[, 'X1'], theta_hat, col = 'darkgreen', lwd = 2)
  lines(X[, 'X1'], theta(X), col = 'blue', lty=2)
  #legend("bottomright", legend = c("True Theta", "Theta Tilde", "Theta Hat"), 
   #      col = c("blue", "red", "green"), lty = c(2, 1, 1), cex = 0.8)
  dev.off()
  
}

# Print RMSE results
print(rmse_results)

