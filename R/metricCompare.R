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

#' Compare two non-AUC metrics at either a classification threshold or a constraint
#'
#' @param predictions1 A vector of predictions in (0, 1) from estimator 1
#' @param predictions2 A vector of predictions in (0, 1) from estimator 2
#' @param labels A vector of binary truth labels
#' @param metric The metric of interest, one of ("sens", "spec", "ppv", "npv")
#' @param threshold_type One of ("prob", "sens", "spec", "total_pos"), and not equal to metric
#' @param threshold The value of the threshold to use
#' @param confidence Confidence level for CI
#' @param se_type One of c("binomial", "logit"), defaults to "binomial"
#'
#' @return
#' @export
#'
#' @examples
compare_metric <- function(predictions1,
                           predictions2,
                           labels,
                           metric,
                           threshold_type,
                           threshold,
                           confidence = 0.95,
                           se_type = "binomial"){
  #Check Inputs
  .check_metric_inputs(predictions1 = predictions1,
                       predictions2 = predictions2,
                       labels = labels,
                       metric = metric,
                       threshold_type = threshold_type,
                       threshold = threshold,
                       confidence = confidence)

  ### Get Confusion Matrices at specified Threshold
  cm1 <- get_cm_at_threshold(predictions1, labels, threshold_type, threshold)
  cm2 <- get_cm_at_threshold(predictions2, labels, threshold_type, threshold)

  # Summary Tables
  metric1 = unlist(summarize_metric(predictions1,
                           labels = labels,
                           metric = metric,
                           threshold_type = threshold_type,
                           threshold = threshold,
                           confidence = confidence))
  m1 = metric1[1]
  metric2 = unlist(summarize_metric(predictions2,
                           labels = labels,
                           metric = metric,
                           threshold_type = threshold_type,
                           threshold = threshold,
                           confidence = confidence))
  m2 = metric2[1]


  #Comparisons
  diff = m1 - m2
  ratio = m1/m2
  log_ratio = log(m1) - log(m2)

  return(list(summary = data.frame(rbind(metric1, metric2)),
              comparison = c(diff = diff, ratio = ratio, log_ratio = log_ratio)))
}


#' Summarize a single estimator's performance
#'
#' @param predictions A vector of predictions in (0, 1) from an estimator
#' @param labels A vector of truth labels (the outcomes, in {0,1})
#' @param metric The metric of interest, one of ("sens", "spec", "ppv", "npv")
#' @param threshold_type One of ("prob", "sens", "spec", "total_pos"), and not equal to metric
#' @param threshold The value of the threshold to use
#' @param confidence Confidence level for CI
#' @param se_type One of c("binomial", "logit"), defaults to "binomial"
#'
#' @return
#' @export
#'
#' @examples
summarize_metric <- function(predictions,
                             labels,
                             metric,
                             threshold_type,
                             threshold,
                             confidence = 0.95,
                             se_type = "binomial"){

  .check_met_summary_inputs(predictions = predictions,
                            labels = labels,
                            metric = metric,
                            threshold_type = threshold_type,
                            threshold = threshold)

  ### Get Confusion Matrix at specified Threshold
  cm <- get_cm_at_threshold(predictions, labels, threshold_type, threshold)

  ### Get Metric of Interest Summary
  metric = cm[metric]
  metric_se = get_metric_se(n = length(predictions),
                             p = metric,
                             se_type = se_type)
  metric_ci = get_ci(mean = unlist(metric), se = unlist(metric_se), confidence = confidence)

  summary = data.frame(c(target = metric, se = metric_se, ci_lower = metric_ci[1], ci_upper = metric_ci[2]))
  return(summary)
}

# Function to get binomial standard error estimate for proportion p, sample size n
get_binomial_se <- function(n, p){
  sqrt((1/n)*p*(1-p))
}

#Function to get confidence interval given se, confidence level using normal approx
get_ci <- function(mean, se, confidence) {
  return(unlist( c(mean + se*qnorm((1-confidence)/2), mean - se*qnorm((1-confidence)/2))))
}

# Function to get se on logit scale
get_logit_se <- function(n, p){
  NULL
}

# Function to get IC based inference for sens, spec, ppv, npv differences
get_ic_inference <- function(n, psi1, psi2, ic1, ic2){
  #Requires derivation of influence function
  NULL
}

# Function to get a logit-transform based confidence interval for 1 estimator
get_logit_ci <- function(p, confidence){
  NULL
}

# Return Variance and Standard Error of a Specified Metric
# confusion_mat is a dataframe with columns named
# metric is the name of the column of interest
get_metric_se <- function(n, p, se_type = "binomial"){
  if (se_type  == "binomial"){
    se = get_binomial_se(n, p)
  } else if (se_type == "logit"){
    se = get_logit_se(n, p)
  } else {stop("Invalid inference type specified; must be one of binomial,
               logit")
  }
  return(se)
}

# Function to check inputs for metric comparison
.check_metric_inputs <- function(predictions1, predictions2, labels,
                     metric, threshold_type, threshold, confidence){
  stopifnot(metric != threshold_type)
  stopifnot(metric %in% c("sens", "spec", "ppv", "npv"))
  stopifnot(threshold_type %in% c("sens", "spec", "prob"))
  stopifnot(threshold > 0 & threshold < 1)
  stopifnot(confidence > 0 & confidence < 1)
  stopifnot(length(predictions1) == length(predictions2))
  stopifnot(length(labels) == length(predictions1))
}

# Function to check inputs for metric summary
.check_met_summary_inputs <- function(predictions = predictions,
                                      labels = labels,
                                      metric = metric,
                                      threshold_type = threshold_type,
                                      threshold = threshold){
  stopifnot(metric != threshold_type)
  stopifnot(metric %in% c("sens", "spec", "ppv", "npv"))
  stopifnot(threshold_type %in% c("sens", "spec", "prob"))
  stopifnot(threshold > 0 & threshold < 1)
  stopifnot(length(labels) == length(predictions))
}
