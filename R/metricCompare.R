# Sensitivity, Specificity, PPV, NPV Calculator and Comparison Tool
#
# This function calculates a comparison of two classifiers based on a specified
# metric., either sensitivity, specificity, PPV, or NPV. These metrics are
# require specification of a classification threshold, which can be provided as
# either a predicted probability of the binary outcome (i.e. a value of the
# score), or as a total positive rate (i.e. the threshold is chosen such that
# X percent of the observations are classified as positives, or as a constraint
# on another of the metrics (e.g. sensitivity)

# These comparisons use parametric assumptions for inference and the empirical
# distribution for estimation.

#' Compare two non-AUC metrics
#'
#' @param predictions1 A vector of predictions in (0, 1) from estimator 1
#' @param predictions2 A vector of predictions in (0, 1) from estimator 2
#' @param labels A vector of truth labels (the outcomes, in {0,1})
#' @param comparison Name of comparison, one of ("diff", "ratio", "log_ratio")
#' @param metric The metric of interest, one of ("sens", "spec", "ppv", "npv")
#' @param threshold_type One of ("prob", "sens", "spec", "total_pos"), and not equal to metric
#' @param threshold The value of the threshold to use
#' @param confidence Confidence level for CI
#'
#' @return
#' @export
#'
#' @examples
compare_metric <- function(predictions1, predictions2, labels, comparison,
                           metric, threshold_type, threshold, confidence = 0.95){
  #Check Inputs
  .check_metric_inputs(predictions1, predictions2, labels, comparison,
                       metric, threshold_type, threshold, confidence)

  ### Get Confusion Matrices at specified Threshold
  cm1 <- get_cm_at_threshold(predictions1, labels, threshold_type, threshold)
  cm2 <- get_cm_at_threshold(predictions2, labels, threshold_type, threshold)

  ### Get Comparison of Interest for Specified Metric and Inference Using DM
  if (comparison == "diff"){
  NULL
  } else if (comparison == "ratio"){
  NULL
  } else if (comparison == "log_ratio"){
  NULL
  }
}


# Function to get binomial confidence interval
get_binomial_se <- function(n, p){
  sqrt((1/n)*p*(1-p))
}


# Function to get se on logit scale
get_logit_se <- function(n, p){
  NULL
}

# Function to get a logit-transform based confidence interval
get_logit_ci <- function(z, se, confidence){
  eta = log(p/1-p) #Translate using logit function
  se_eta = (n*p*(1-p))^(-1/2)
  eta_int = c(eta - z*se_eta, eta + z*se_eta)
  return(c(plogis(eta - z*se_eta), plogis(eta + z*se_eta)))
}

# Return Variance and Standard Error of a Specified Metric
# confusion_mat is a dataframe with columns named
# metric is the name of the column of interest
get_metric_se <- function(predictions, labels, confusion_mat, metric, type){
  n = length(labels)
  p = confusion_mat[metric]
  if (type  == "binomial"){
    se = get_binomial_se(n, p)
  } else if (type == "logit"){
    se = get_logit_se(n, p)
  } else {stop("Invalid inference type specified; must be one of binomial,
               logit")
  }
  se
}

# Function to check a valid confusion matrix is passed in
.check_metric_inputs <- function(predictions1, predictions2, labels, comparison,
                     metric, threshold_type, threshold, confidence){
  stopifnot(metric != threshold_type)
  stopifnot(metric %in% c("sens", "spec", "ppv", "npv"))
  stopifnot(threshold_type %in% c("sens", "spec", "prob"))
  stopifnot(threshold >= 0 & threshold <= 1)
  stopifnot(confidence > 0 & confidence < 1)
  stopifnot(length(predictions1) == length(predictions2))
  stopifnot(length(labels) == length(predictions1))
}
