# Calculate Confidence Intervals

### Testing Code ###
test_preds = runif(1000)
test_labels = rbinom(n = 1000, size = 1, p = test_preds)
cm_at_sens(test_preds, test_labels, sens = .8)
cmat <- cm_at_prob(test_preds, test_labels, prob = .5)

get_metric_se(test_preds, test_labels, cmat, metric = "spec", type = "binomial")
