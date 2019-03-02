### Some code to test the functions
set.seed(1222)
nobs = 5100

generate_data <- function(nobs){
  X1 = runif(n= nobs)
  X2 = runif(n= nobs)
  X3 = rnorm(n = nobs)
  Y = floor(2*(1-plogis(.2*X1 + .5*X2 - X3)))
  O = data.frame(Y, X1, X2, X3)
  return(O)
}
O = generate_data(nobs)

#Fake 1:
fit1 <- glm(Y ~ X1 + X2, data = O, family = "binomial")
p1 <- predict(fit1, newdata = O, type="response")
#Fake 2:
fit2 <- glm(Y ~ X1, data = O, family = "binomial")
p2 <- predict(fit2, newdata = O, type="response")

# Get cvAUC estimates and ICs
cvAUC1 <- cvAUC_ic(predictions1 = p1, labels = O$Y, confidence = 0.95)
cvAUC2 <- cvAUC_ic(predictions1 = p2, labels = O$Y, confidence = 0.95)

# Comparison Tests
compare_cvAUC(predictions1 = p1,
              predictions2 = p2,
              labels = O$Y,
              label.ordering = NULL,
              folds = NULL,
              confidence = 0.95,
              comparison = "diff")

compare_cvAUC(predictions1 = p1,
              predictions2 = p2,
              labels = O$Y,
              label.ordering = NULL,
              folds = NULL,
              confidence = 0.95,
              comparison = "ratio")

# Test Sample Metric Comparisons
met_list = c("ppv", "npv", "sens", "spec")
cons_list = c("sens", "spec")
ci_list = c("binomial", "logit")
for (met in met_list){
  for (cons in cons_list){
    for (ci_type in ci_list){
      if (met != cons){
        print(c(met, cons, ci_type))

        print(compare_metric(predictions1 = p1,
                             predictions2 = p2,
                             labels = O$Y,
                             metric = met,
                             threshold_type = cons,
                             threshold = .5,
                             ci_type = ci_type))
      }
    }
  }
}
