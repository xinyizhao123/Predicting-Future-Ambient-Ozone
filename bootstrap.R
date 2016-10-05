# Return predicted values from bootstrap
mySumm <- function(.) {
  predict(., newdata=test, re.form=NULL)
}
# Collapse bootstrap into median, 95% PI
sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
  )
}

boot1 <- lme4::bootMer(fit4, mySumm, nsim=1000, use.u=FALSE, type="parametric")
PI.boot1 <- sumBoot(boot1)