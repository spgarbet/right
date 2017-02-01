load("~/Downloads/raw_all10_1.rda")

n <- length(unique(results$name))
x <- table(subset(results, resource=="st_event")$name)

y <- c(x, rep(0, n-length(x)))
sum(y)

# Look at distribution
h <- hist(y, breaks=0:20 - 0.5)

library(MASS)
est <- fitdistr(y, "negative binomial")
round(h$density, 3)
round(dnbinom(0:5, size=est$estimate['size'], mu=est$estimate['mu']),3)
# Decent parameterization using negative binomial

# Now for the 95% CI, i.e. the expected variance in the count.
n*confint(est)[2,]

# If you really want to be conservative, use the 99%
n*confint(est,level=0.99)[2,]
