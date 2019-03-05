# cvAUCcompare

Package currently in development - not stable/complete

Package for comparing cross-validated AUC (area under ROC curve) estimates, cross-validated AUPRC (area under precision-recall curve) estimates, and other common binary classifier metrics.

Comparisons for AUC and AUPRC use influence function estimates and the Delta Method for inference. Future developments will incorporate non-parametric approach (U-statistics) used in DeLong 1988 paper on comparing AUC estimates.

Comparisons for other metrics use one of several standard parametric assumptions - i.e. assuming that the positive predictive value, negative predictive value, sensitivity, or specificity are a binomial proportion and using the normal approximation of a binomial random variable. Future developments could develop influence curve-based approaches to these parameters.

## Comparing AUC Using Influence Functions

The influence function for a V-fold cross validated estimate of AUC is established in previous work (LeDell 2015), and this package incorporates functions from the resulting cvAUC package, namely the internal function used to calculate the influence function for observations within a cross-validation fold. The Covariance matrix of the influence functions for two estimators can be used with the Delta Method to estimate the asymptotic variance of contrasts of those estimators. This accounts for correlation between the two cvAUC estimators.

## Comparing AUPRC (Area Under Precision Recall Curve) Using Influence Functions

Currently deriving the influence function and extending to V-fold cross-validation. 

Future work is using influence function(s) to compare AUPRCs.

## Comparing Metrics at a Specified Probability Threshold or other Constraint

