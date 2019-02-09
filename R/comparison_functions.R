# Utility Functions
#
# This file contains functions to calculate differences, ratios and log-ratios
# of cross-validated AUC estimates as well as the variance of the comparisons
# using either a first order or second order delta method.
#
#

# Function for First-Order Delta Method Variance
#
delta_method <- function(grad, Sigma){
  t(grad)%*%Sigma%*%grad
}

# Difference
# Function to Compute the Difference in AUC and the variance of the difference
difference <- function(auc1, auc2, ic1, ic2){
  n_obs = length(ic1)
  h = auc1 - auc2
  #Gradient of h(auc1, auc2) and Covariance Matrix of ICs
  grad = c(1, -1)
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  var_h = (1/nobs)*delta_method(grad, Sigma)
  return(list(h = h, var = var_h))
}

# Ratio
# Function to Compute the Ratio in AUC and the variance of the difference
ratio <- function(auc1, auc2, ic1, ic2){
  n_obs = length(ic1)
  h = auc1/auc2

  #Gradient of h(auc1, auc2) and Covariance Matrix of ICs
  grad = c(1/auc2, -auc1/(auc2^2))
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  var_h = (1/nobs)*delta_method(grad, Sigma)
  return(list(h = h, var_h = var_h))
}

# Log-Ratio
# Function to Compute the log-ratio in AUC and the variance of the log ratio
logratio <- function(auc1, auc2, ic1, ic2){
  n_obs = length(ic1)
  h = log(auc1) - log(auc2)

  #Gradient of h(auc1, auc2) and Covariance Matrix of ICs
  grad = c(1/auc1, -1/auc2)
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  var_h = (1/nobs)*(1/nobs)*delta_method(grad, Sigma)
  return(list(h = h, var_h = var_h))
}
