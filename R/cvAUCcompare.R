# cvAUCcompare
#
# This file computes a specified comparison of two cross-validated AUC estimates
# given two sets of scores, one set of outcomes and one set of folds. The folds
# and outcomes must be the same for both cvAUC estimates.
#
source("./R/cvAUC_ic.R")
source("./R/internals.R")
source("./R/comparison_functions.R")

#' Compare two cvAUC Estimates
#'
#' @param predictions1 A vector of predictions in (0, 1) from estimator 1
#' @param predictions2 A vector of predictions in (0, 1) from estimator 2
#' @param labels A vector of truth labels (the outcomes, in {0,1})
#' @param label.ordering
#' @param folds A vector of fold IDs or a list of folds
#' @param confidence Confidence level for CI
#' @param comparison Name of comparison, one of ("diff", "ratio", "log ratio")
#'
#' @return List of comparison name, metric, variance, se, CI of metric,
#' @export
#'
#' @examples
cvAUCcompare <- function(predictions1, predictions2,
                         labels, label.ordering, folds=NULL,
                         confidence = 0.95, comparison = "diff"){
  # Obtain cvAUC Estimates and Influence Curves
  cvAUC1 = cvAUC_ic(predictions1, labels, label.ordering, folds, confidence)
  cvAUC2 = cvAUC_ic(predictions2, labels, label.ordering, folds, confidence)

  # Get One of 3 Measures of Interest
  if(comparison == "diff"){
    res = difference(cvAUC1[[1]], cvAUC2[[1]], cvAUC1[[2]], cvAUC2[[2]])
  } else if (comparison == "ratio"){
    res = ratio(cvAUC1[[1]], cvAUC2[[1]], cvAUC1[[2]], cvAUC2[[2]])
  } else if (comparison == "log_ratio"){
    res = logratio(cvAUC1[[1]], cvAUC2[[1]], cvAUC1[[2]], cvAUC2[[2]])
    #Note should push CIs back onto original scale too
  } else {stop("Invalid comparison specified; must be one of diff, ratio, log_ratio") }

  z = -qnorm((1- confidence)/2)
  h = res[[1]]
  se_h = sqrt(res[[2]])
  return(list(comparison = comparison, h = h, var_h = res[[2]],
              se_h = se_h, ci_l = h-z*se_h, ci_u = h+z*se_h))
}
