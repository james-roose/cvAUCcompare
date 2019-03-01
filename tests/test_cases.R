files <- list.files("./R/")
for (f in files[1:5]) {
  source(file.path("./R", f))
}

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
# folds_temp = c(rep(1,200), rep(2,200), rep(3,200), rep(4,200), rep(5,200))
# test_folds = list(`1` = which(folds_temp==1), `2` = which(folds_temp==2),
#                   `3` = which(folds_temp==3),
#              `4` = which(folds_temp==4), `5` = which(folds_temp==5))
#Fake 1:
fit1 <- glm(Y ~ X1 + X2, data = O, family = "binomial")
p1 <- predict(fit1, newdata = O, type="response")
#Fake 2:
fit2 <- glm(Y ~ 1, data = O, family = "binomial")
p2 <- predict(fit2, newdata = O, type="response")

cvAUC1 <- cvAUC_ic(predictions1 = p1, labels = O$Y, confidence = 0.95)
cvAUC2 <- cvAUC_ic(predictions1 = p2, labels = O$Y, confidence = 0.95)

cvAUCcompare(predictions1 = p1,
             predictions2 = p2,
             labels = O$Y,
             label.ordering = NULL,
             folds = NULL,
             confidence = 0.95,
             method = "diff")
