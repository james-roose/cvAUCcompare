# Internal functions to calculate confusion matrices

# Get CM at Specified threshold value of threshold_type
get_cm_at_threshold <- function(predictions, labels, threshold_type, threshold){
  if (threshold_type == "prob"){
    cm <- cm_at_prob(predictions, labels, threshold)
  } else if (threshold_type == "sens"){
    cm <- cm_at_sens(predictions, labels, threshold)
  } else if (threshold_type == "spec"){
    cm <- cm_at_spec(predictions, labels, threshold)
  } else if (threshold_type == "total_pos"){
    cm <- cm_at_total_pos(predictions, labels, threshold)
  }
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
cm_at_total_pos <- function(predictions, labels, total_pos){
  preds_ordered = predictions[order(predictions, decreasing = T)]
  prob = preds_ordered[total_pos]
  return(cm_at_prob(predictions, labels, prob))
}
