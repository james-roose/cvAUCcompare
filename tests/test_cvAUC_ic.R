# Quick test
X = as.matrix(rnorm(1000), nrow = 1000)
Y = rbinom(n = 1000, size = 1, p = 0.2)
SL <- CV.SuperLearner(Y, data.frame(X), SL.library = c("SL.mean", "SL.glm"), V = 10)

folds = SL$folds
predictions = SL$SL.predict
labels = SL$Y

cvAUC_ic(predictions1 = predictions, folds = folds, labels = labels)
