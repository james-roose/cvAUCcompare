# cvAUC_ic
#
# This function calculates the cross-validated AUC estimate and its influence
# curve, variance, se and CI from a vector of predictions, a vector of true
# outcomes and a list or vector of fold labels, as well as a confidence level
source("./R/internals.R")
cvAUC_ic <- function(predictions1, labels, label.ordering = NULL, folds = NULL,
                     confidence = 0.95) {

  # Pre-process the input
  #Prediction Algo 1:
  clean1 <- .process_input(predictions = predictions1, labels = labels,
                           label.ordering = label.ordering, folds = folds,
                           ids = NULL, confidence = confidence)

  predictions1 <- clean1$predictions  # Length-V list of predicted values
  labels1 <- clean1$labels  # Length-V list of true labels
  pos1 <- levels(labels1[[1]])[[2]]  # Positive class label
  neg1 <- levels(labels1[[1]])[[1]]  # Negative class label
  n_obs1 <- length(unlist(labels1))  # Number of observations

  # Inverse probability weights across entire data set
  w1_1 <- 1/(sum(unlist(labels1) == pos1)/n_obs1)  # Inverse weights for positive class
  w0_1 <- 1/(sum(unlist(labels1) == neg1)/n_obs1)  # Inverse weights for negative class

  # This is required to cleanly get past R CMD CHECK
  # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  pred <- label <- NULL
  fracNegLabelsWithSmallerPreds <- fracPosLabelsWithLargerPreds <- icVal <- NULL

  #Calculate Influence Curve for a Single Function
  .IC <- function(fold_preds, fold_labels, pos, neg, w1, w0) {
    # Applied to a single fold's (preds, labels)
    n_rows <- length(fold_labels)
    n_pos <- sum(fold_labels == pos)
    n_neg <- n_rows - n_pos
    auc <- cvAUC::AUC(fold_preds, fold_labels)
    DT <- data.table(pred = fold_preds, label = fold_labels)
    DT <- DT[order(pred, -xtfrm(label))]  #Sort by asc(pred), desc(label)
    DT[, fracNegLabelsWithSmallerPreds := cumsum(label == neg)/n_neg]
    DT <- DT[order(-pred, label)]
    DT[, fracPosLabelsWithLargerPreds := cumsum(label == pos)/n_pos]
    DT[, icVal := ifelse(label == pos, w1 * (fracNegLabelsWithSmallerPreds - auc),
                         w0 * (fracPosLabelsWithLargerPreds - auc))]
    return(DT$icVal)
  }

  # Estimate IC for both Prediction Algorithms
  ic1 <- mapply(FUN = .IC, fold_preds = predictions1,
                fold_labels = labels1,
                MoreArgs = list(pos = pos1, neg = neg1, w1 = w1_1, w0 = w0_1))



  # Get cvAUC Value for Each
  # Requires cvAUC function
  cvauc1 <- cvAUC::cvAUC(predictions1, labels1, folds = folds1)$cvAUC

  #Return Inference
  var = mean(ic1^2)
  se = sqrt(var)
  z = -qnorm((1- confidence)/2)
  ci_l = cvauc1 - z*se
  ci_u = cvauc1 + z*se

  list(cvauc1 = cvauc1, var = var, se = se, ci_l = ci_l,
       ci_u = ci_u, ic1 = ic1)
}
