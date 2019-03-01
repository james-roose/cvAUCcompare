# cvAUCcompare

Package currently in development - not stable/complete

Package for comparing cross-validated AUC (area under ROC curve) estimates, cross-validated AUPRC (area under precision-recall curve) estimates, and other common binary classifier metrics.

Comparisons for AUC and AUPRC use influence function estimates and the Delta Method for inference. Future developments will incorporate non-parametric approach (U-statistics) used in DeLong 1988 paper on comparing AUC estimates.

Comparisons for other metrics use one of several standard parametric assumptions.

## Comparing AUC Using Influence Functions

The influence function for a V-fold cross validated estimate of AUC is established in previous work (LeDell 2015), and this package incorporates functions from the resulting cvAUC package.

## Comparing AUPRC (Area Under Precision Recall Curve) Using Influence Functions

Deriving and computing the influence function

Calculating the cross-validated AUPRC and influence function estkmates

Using influence function(s) to compare AUPRCs

## Comparing Metrics at a Specified Probability Threshold or other Constraint

