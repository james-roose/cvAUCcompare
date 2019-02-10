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
  #Calculate Confusion Matrix and Metrics of Interest
  #Get Metric and Inference
NULL
}
