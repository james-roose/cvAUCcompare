# Sensitivity, Specificity, PPV, NPV Calculator
#
# This function calculates a comparison of two classifiers based on a specified
# metric., either sensitivity, specificity, PPV, or NPV. These metrics are
# require specification of a classification threshold, which can be provided as
# either a predicted probability of the binary outcome (i.e. a value of the
# score), or as a total positive rate (i.e. the threshold is chosen such that
# X percent of the observations are classified as positives.
#
#
compare_metric <- function(predictions1, predictions2, labels, comparison,
                           metric, threshold_type, threshold, confidence = 0.95){
  #Check Inputs
  #If using total positive type, find threshold in terms of score
  #Calculate Confusion Matrix and Metrics of Interest
  #Get Comparisons and Inference via Delta Method
  NULL
}

calculate_metric <- function(predictions, labels, comparison, metric,
                             threshold_type, threshold,  confidence = 0.95){
  #Check Inputs
  #If using total positive type, find threshold in terms of score
  if (threshold_type = "prob"){
    prob_threshold = threshold
  } else if (threshold_type = "total_pos"){
    prob_threshold = quantile(predictions, 1 - threshold)
  } else {stop("Invalid threshold type specified; must be one of prob, total_pos")}
  pos_preds = as.numeric(predictions > threshold)
  neg_preds = as.numeric(predictions < threshold)
  # Confusion Matrix
  TP = sum(pos_preds*labels)
  FP = sum(pos_preds) - TP
  TN = sum(neg_preds*(labels==0))
  FN = sum(neg_preds) - TN
  # Get Requested Metrics
  ppv = TP/(TP + FP)
  npv = TN/(TN + FN)
  sens = TP/(TP + FN)
  spec = TN/(TN + FP)
  # Standard Error Estimates
  NULL
}
