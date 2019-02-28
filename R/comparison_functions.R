# Utility Functions
#
# This file contains functions to calculate differences, ratios and log-ratios
# of cross-validated estimates as well as the variance of the comparisons
# using estimated influence curves
#

#' Function for First-Order Delta Method Variance
#'
#' @param grad
#' @param Sigma
#'
#' @return
#' @export
#'
#' @examples
delta_method <- function(grad, Sigma){
  stopifnot(is.Matrix(Sigma))
  t(grad)%*%Sigma%*%grad
}

# Difference
# Function to Compute the Difference and the variance of the difference
difference <- function(psi1, psi2, ic1, ic2){
  .check_inputs(psi1, psi2, ic1, ic2)
  n_obs = length(ic1)
  h = psi1 - psi2
  #Gradient of h(psi1, psi2) and Covariance Matrix of ICs
  grad = c(1, -1)
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  var_h = (1/nobs)*delta_method(grad, Sigma)
  return(list(h = h, var = var_h))
}

# Ratio
# Function to Compute the Ratio in and the variance of the Ratio
ratio <- function(psi1, psi2, ic1, ic2){
  .check_inputs(psi1, psi2, ic1, ic2)
  n_obs = length(ic1)
  h = psi1/psi2

  #Gradient of h(psi1, psi2) and Covariance Matrix of ICs
  grad = c(1/psi2, -psi1/(psi2^2))
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  var_h = (1/nobs)*delta_method(grad, Sigma)
  return(list(h = h, var_h = var_h))
}

# Log-Ratio
# Function to Compute the log-ratio and the variance of the log ratio
logratio <- function(psi1, psi2, ic1, ic2){
  .check_inputs(psi1, psi2, ic1, ic2)
  n_obs = length(ic1)
  h = log(psi1) - log(psi2)

  #Gradient of h(psi1, psi2) and Covariance Matrix of ICs
  grad = c(1/psi1, -1/psi2)
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  var_h = (1/nobs)*delta_method(grad, Sigma)
  return(list(h = h, var_h = var_h))
}

# Function to check inputs for 3 IC based comparison functions
.check_inputs <- function(psi1, psi2, ic1, ic2) {
  stopifnot(is.numeric(psi1) & (psi1 < 1) & (psi1 > 0))
  stopifnot(is.numeric(psi2) & (psi2 < 1) & (psi2 > 0))
  stopifnot(is.numeric(ic1) & (is.numeric(ic2)) &
              length(ic1 == ic2) & (ic1 != ic2))
}
