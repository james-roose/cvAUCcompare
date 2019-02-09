# cvAUCcompare
#
# This file computes a specified comparison of two cross-validated AUC estimates
# given two sets of scores, one set of outcomes and one set of folds. The folds
# and outcomes must be the same for both cvAUC estimates.
#

source("./R/cvAUC_ic.R")
source("./R/internals.R")


cvAUCcompare <- function(predictions1, predictions2,
                         labels, label.ordering, folds=NULL){
  # Obtain cvAUC Estimates and Influence Curves
  cvAUC1 = cvAUC_ic(predictions1, labels, label.ordering, folds)
  cvAUC2 = cvAUC_ic(predictions2, labels, label.ordering, folds)

  # Get 3 Measures of Interest
  diff = cvAUC_difference(cvAUC1[[1]], cvAUC2[[1]], cvAUC1[[2]], cvAUC2[[2]])
  ratio = cvAUC_ratio(cvAUC1[[1]], cvAUC2[[1]], cvAUC1[[2]], cvAUC2[[2]])
  log_ratio = cvAUC_logratio(cvAUC1[[1]], cvAUC2[[1]], cvAUC1[[2]], cvAUC2[[2]])

  return(list(diff = diff, ratio = ratio, log_ratio = log_ratio))
}
