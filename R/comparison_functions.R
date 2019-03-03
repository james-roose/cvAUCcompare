# Utility Functions
#
# This file contains functions to calculate differences, ratios and log-ratios
# of cross-validated estimates as well as the variance of the comparisons
# using estimated influence curves
#

#' Apply Delta Method to Difference
#'
#' @param psi1 Estimate 1 of a parameter of interest
#' @param psi2 Estimate 2 of a parameter of interest
#' @param ic1 Vector of influence curve estimates for estimator 1
#' @param ic2 Vector of influence curve estimates for estimator 2
#'
#' @return Difference and variance of difference
#' @export
#'
#' @examples
difference <- function(psi1, psi2, ic1, ic2){
  .check_inputs(psi1, psi2, ic1, ic2)
  n_obs = length(ic1)
  h = psi1 - psi2
  #Gradient of h(psi1, psi2) and Covariance Matrix of ICs
  grad = c(1, -1)
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  var_h = (1/n_obs)*.delta_method(grad, Sigma)
  return(list(h = h, var = var_h))
}

#' Apply Delta Method to Ratio
#'
#' @param psi1 Estimate 1 of a parameter of interest
#' @param psi2 Estimate 2 of a parameter of interest
#' @param ic1 Vector of influence curve estimates for estimator 1
#' @param ic2 Vector of influence curve estimates for estimator 2
#'
#' @return Ratio and variance of ratio
#' @export
#'
#' @examples
ratio <- function(psi1, psi2, ic1, ic2){
  .check_inputs(psi1, psi2, ic1, ic2)
  n_obs = length(ic1)
  h = psi1/psi2

  #Gradient of h(psi1, psi2) and Covariance Matrix of ICs
  grad = c(1/psi2, -psi1/(psi2^2))
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  var_h = (1/n_obs)*.delta_method(grad, Sigma)
  return(list(h = h, var_h = var_h))
}

#' Apply Delta Method to Log Ratio
#'
#' @param psi1 Estimate 1 of a parameter of interest
#' @param psi2 Estimate 2 of a parameter of interest
#' @param ic1 Vector of influence curve estimates for estimator 1
#' @param ic2 Vector of influence curve estimates for estimator 2
#'
#' @return Log ratio and variance of log ratio
#' @export
#'
#' @examples
logratio <- function(psi1, psi2, ic1, ic2){
  .check_inputs(psi1, psi2, ic1, ic2)
  n_obs = length(ic1)
  h = log(psi1) - log(psi2)

  #Gradient of h(psi1, psi2) and Covariance Matrix of ICs
  grad = c(1/psi1, -1/psi2)
  Sigma = cov(as.matrix(cbind(ic1, ic2)))

  var_h = (1/n_obs)*.delta_method(grad, Sigma)
  return(list(h = h, var_h = var_h))
}

# Function for First-Order Delta Method Variance
.delta_method <- function(grad, Sigma){
  stopifnot(is.matrix(Sigma))
  t(grad)%*%Sigma%*%grad
}

# Function to check inputs for 3 IC based comparison functions
.check_inputs <- function(psi1, psi2, ic1, ic2) {
  stopifnot(is.numeric(psi1) & (psi1 < 1) & (psi1 > 0))
  stopifnot(is.numeric(psi2) & (psi2 < 1) & (psi2 > 0))
  stopifnot(is.numeric(ic1) & (is.numeric(ic2)) &
              length(ic1) == length(ic2))
}
