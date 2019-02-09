# Utility Functions
#
# This file contains functions to calculate differences, ratios and log-ratios
# of cross-validated AUC estimates as well as the variance of the comparisons
# using either a first order or second order delta method.
#
#

# Functions for First-Order Delta Method Variance

# Difference
# Function to Compute the Difference in AUC and the variance of the difference
cvAUC_difference <- function(auc1, auc2, ic1, ic2){
  n_obs = length(ic1)
  diff = auc1 - auc2
  #Gradient of h(auc1, auc2) and Covariance Matrix of ICs
  grad = c(1, 1)
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  diff_var = (1/nobs)*(grad*Sigma*grad)
  diff_se = sqrt(diff_var)
  return(list(diff = diff, var = diff_var))
}

# Ratio
# Function to Compute the Ratio in AUC and the variance of the difference
cvAUC_ratio <- function(auc1, auc2, ic1, ic2){
  ratio = auc1/auc2
  n_obs = length(ic1)

  #Gradient of h(auc1, auc2) and Covariance Matrix of ICs
  grad = c(1/auc2, -auc1/(auc2^2))
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  ratio_var = (1/nobs)*(grad*Sigma*grad)
  ratio_se = sqrt(ratio_var)
  return(list(ratio = ratio, var = ratio_var))
}

# Log-Ratio
# Function to Compute the log-ratio in AUC and the variance of the log ratio
cvAUC_logratio <- function(auc1, auc2, ic1, ic2){
  log_ratio = log(auc1) - log(auc2)
  n_obs = length(ic1)

  #Gradient of h(auc1, auc2) and Covariance Matrix of ICs
  grad = c(1/auc1, 1/auc2)
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  lr_var = (1/nobs)*(1/nobs)*(grad*Sigma*grad)
  lr_se = sqrt(lr_var)
  return(list(log_ratio = log_ratio, var = lr_var))
}
