# cvAUCcompare
#
# This file computes a specified comparison of two cross-validated AUC estimates
# given two sets of scores, one set of outcomes and one set of folds. The folds
# and outcomes must be the same for both cvAUC estimates.
#
source("./R/cvAUC_ic.R")
source("./R/internals.R")

cvAUCcompare <- function(predictions1, predictions2,
                         labels, label.ordering, folds=NULL,
                         confidence = 0.95, method = "diff"){
  # Obtain cvAUC Estimates and Influence Curves
  cvAUC1 = cvAUC_ic(predictions1, labels, label.ordering, folds, confidence)
  cvAUC2 = cvAUC_ic(predictions2, labels, label.ordering, folds, confidence)

  # Get One of 3 Measures of Interest
  if(method == "diff"){
    res = cvAUC_difference(cvAUC1[[1]], cvAUC2[[1]], cvAUC1[[2]], cvAUC2[[2]])
  } else if (method == "ratio"){
    res = cvAUC_ratio(cvAUC1[[1]], cvAUC2[[1]], cvAUC1[[2]], cvAUC2[[2]])
  } else if (method == "log_ratio"){
    res = cvAUC_logratio(cvAUC1[[1]], cvAUC2[[1]], cvAUC1[[2]], cvAUC2[[2]])
    #Note should push CIs back onto original scale too
  }

  z = -qnorm((1- confidence)/2)
  h = res[[1]]
  se_h = sqrt(res[[2]])
  return(list(method = method, h = h, var_h = res[[2]],
              se_h = se_h, ci_l = h-z*se_h, ci_u = h+z*se_h))
}
