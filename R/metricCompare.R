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

calculate_metric <- function(predictions,
                             labels,
                             metric = "ppv",
                             threshold_type,
                             threshold,
                             inference,
                             confidence = 0.95){
  #Check Inputs
  #If using total positive type, find threshold in terms of score
  if (threshold_type == "prob"){
    prob_threshold = threshold
  } else if (threshold_type == "total_pos"){
    prob_threshold = quantile(predictions, 1 - threshold)
  } else {stop("Invalid threshold type specified; must be one of prob, total_pos")}
  pos_preds = as.numeric(predictions > prob_threshold)
  neg_preds = as.numeric(predictions < prob_threshold)
  # Confusion Matrix
  TP = sum(pos_preds*(labels==1))
  FP = sum(pos_preds) - TP
  TN = sum(neg_preds*(labels==0))
  FN = sum(neg_preds) - TN
  # And derived confusion matrix
  ppv = TP/(TP + FP)
  npv = TN/(TN + FN)
  sens = TP/(TP + FN)
  spec = TN/(TN + FP)

  # Get Requested Metric
  if ( !(metric %in% c("ppv", "npv", "sens", "spec") ) ){
    stop("Invalid metric specified, must be one of ppv, npv, sens, spec")
  }

  # Standard Error Estimates - all of these metrics are in 0/1, one option is
  # to assume they are a binomial parameter and use usual binomial SE calculation
  z = -qnorm((1-confidence)/2)
  n = length(labels)
  p = get(metric)
  se = NULL
  if (inference  == "binomial"){
    se = get_binomial_se(n, p)
    ci = c(p - z*se, p + z*se)
  } else if (inference == "logit"){
    se = NULL
    ci = get_logit_ci(n, p, z)
  } else {stop("Invalid inference type specified; must be one of binomial,
               logit")
  }
  return(list(metric, est = p, var = se^2, se = se, ci_l = ci[1], ci_u = ci[2]))
}

# Function to get binomial confidence interval
get_binomial_se <- function(n, p){
  sqrt((1/n)*p*(1-p))
}

# Function to get a logit-transform based confidence interval
get_logit_ci <- function(n, p, z){
  eta = log(p/1-p) #Translate using logit function
  se_eta = (n*p*(1-p))^(-1/2)
  eta_int = c(eta - z*se_eta, eta + z*se_eta)
  return(c(plogis(eta - z*se_eta), plogis(eta + z*se_eta)))
}

#Some testing code
calculate_metric(predictions = X,
                 labels = Y, metric = "sens",
                 threshold_type = "prob",
                 threshold = .75,
                 inference = "logit")
