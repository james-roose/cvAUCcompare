# cvAUCcompare
#
# This file computes a specified comparison of two cross-validated AUC estimates
# given two sets of scores, one set of outcomes and one set of folds. The folds
# and outcomes must be the same for both cvAUC estimates.
#

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
compare_cvAUC <- function(predictions1,
                          predictions2,
                          labels,
                          label.ordering,
                          folds = NULL,
                          confidence = 0.95,
                          comparison = "diff"){
  # Obtain cvAUC Estimates and Influence Curves
  cvAUC1 = cvAUC_ic(predictions1, labels, label.ordering, folds, confidence)
  cvAUC2 = cvAUC_ic(predictions2, labels, label.ordering, folds, confidence)

  # Should check folds are the same somewhere

  # Get One of 3 Measures of Interest
  if(comparison == "diff"){
    res = difference(cvAUC1$cvauc, cvAUC2$cvauc, ic1 = unlist(cvAUC1$ic), ic2 = unlist(cvAUC2$ic))
    t_stat = (res[[1]] - 0)/sqrt(res[[2]])
  } else if (comparison == "ratio"){
    res = ratio(cvAUC1$cvauc, cvAUC2$cvauc, ic1 = unlist(cvAUC1$ic), ic2 = unlist(cvAUC2$ic))
    t_stat = (res[[1]] - 1)/sqrt(res[[2]])
  } else if (comparison == "log_ratio"){
    res = logratio(cvAUC1$cvauc, cvAUC2$cvauc, ic1 = unlist(cvAUC1$ic), ic2 = unlist(cvAUC2$ic))
    t_stat = (res[[1]] - 1)/sqrt(res[[2]])
    #Note should push CIs back onto original scale too
  } else {stop("Invalid comparison specified; must be one of diff, ratio, log_ratio") }

  z = -qnorm((1- confidence)/2)
  h = res[[1]]
  se_h = sqrt(res[[2]])

  # p-value for difference
  p_val = 2*(1 - pnorm(t_stat))

  return(c(h = h, var_h = res[[2]], se_h = se_h,
           ci_l = h-z*se_h, ci_u = h+z*se_h,
           t_stat = t_stat, p_val = p_val))
}
