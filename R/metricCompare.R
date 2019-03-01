# Sensitivity, Specificity, PPV, NPV Calculator and Comparison Tool
#
# This function calculates a comparison of two classifiers based on a specified
# metric., either sensitivity, specificity, PPV, or NPV. These metrics are
# require specification of a classification threshold, which can be provided as
# either a predicted probability of the binary outcome (i.e. a value of the
# score), or as a total positive rate (i.e. the threshold is chosen such that
# X percent of the observations are classified as positives, or as a constraint
# on another of the metrics (e.g. sensitivity)
#
#


#' Compare two non-AUC metrics
#'
#' @param predictions1 A vector of predictions in (0, 1) from estimator 1
#' @param predictions2 A vector of predictions in (0, 1) from estimator 2
#' @param labels A vector of truth labels (the outcomes, in {0,1})
#' @param comparison Name of comparison, one of ("diff", "ratio", "log ratio")
#' @param metric The metric of interest, one of ("sens", "spec", "ppv", "npv")
#' @param threshold_type The type of threshold, one of ("prob", "v"sens", "spec", "ppv", "npv")
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
  #If using total positive type, find threshold in terms of score
  #Calculate Confusion Matrix and Metrics of Interest
  #Get Comparisons and Inference via Delta Method
  NULL
}

# Return Confusion Matrix and Metrics at a Specified Probability Threshold
cm_at_prob <- function(predictions, labels, prob){
  TP = sum((predictions > prob)*(labels == 1))
  FP = sum((predictions > prob)*(labels == 0))
  FN = sum((predictions <= prob)*(labels == 1))
  TN = sum((predictions <= prob)*(labels == 0))

  # To Return - No Inference Yet
  return(data.frame(tp = TP,
                    fp = FP,
                    fn = FN,
                    tn = TN,
                    ppv = TP/(TP + FP),
                    npv = TN/(TN + FN),
                    sens = TP/(TP + FN),
                    spec = TN/(TN + FP),
                    prob = prob))
}

# Return Confusion Matrix and Metrics at a Specified Sensitivity Level
cm_at_sens <- function(predictions, labels, sens){
  #Determine Required Probability Threshold to Get Desired Sensitivity
  prob = quantile(predictions[labels==1], 1 - sens)
  return(cm_at_prob(predictions, labels, prob))
}

# Return Confusion Matrix and Metrics at a Specified Specificty Level
cm_at_spec <- function(predictions, labels, spec){
  #Determine Required Probability Threshold to Get Desired Specificity
  prob = quantile(predictions[labels==0], 1 - spec)
  return(cm_at_prob(predictions, labels, prob))
}

# Return Confusion Matrix and Metrics at a Specified Number of Positive Preds
cm_at_num_pos <- function(predictions, labels, num_pos){
  preds_ordered = predictions[order(predictions, decreasing = T)]
  prob = preds_ordered[num_pos]
  return(cm_at_prob(predictions, labels, prob))
}

# Return Confusion Matrix and Metrics at a Specified PPV Threshold
cm_at_ppv <- function(predictions, labels, ppv){
  NULL
}

# Return Confusion Matrix and Metrics at a Specified NPV Threshold
cm_at_npv <- function(predictions, labels, npv){
  NULL
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
.check_metric_inputs(predictions1, predictions2, labels, comparison,
                     metric, threshold_type, threshold, confidence = 0.95){
  #Fill in with checks

}
