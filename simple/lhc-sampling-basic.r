require(lhs)
require(tidyverse)

# Parameter List -- This is the list of parameters and their associated distributions for use in the PSA.
pp <- list(
  vRiskA = list(  # This is the risk of Event A (baseline model = 10%)
    type = "beta",
    parameter = "vRiskA",
    shape1 = 100,
    shape2 = 900
  ),
  vFatalA = list( # This is the probability of a fatal event (baseline model = 5%).
    type = "uniform",
    parameter = "vFatalA",
    min = 0.01,
    max = 0.09
  ),
  A_survive = list( # This is the disutility (permanent) associated with Event A (baseline = 0.25 disutility)
    type = "uniform",
    parameter = "A_survive",
    min = 0.20,
    max = 0.30
  ),
  vRiskB = list( # This is the risk of event B happening (Event B only happens if patient experiences A) (baseline = 50%)
    type = "uniform",
    parameter = "vRiskB",
    min = 0.45,
    max = 0.55
  ),
  B = list(  # This is the disutility of event B. (baseline = 0.1)
    type = "uniform",
    parameter = "B",
    min = 0.07,
    max = 0.13
  ),
  vRR = list( # This is the relative risk of Event A happening under the treatment (baseline =0.5)
    type = "beta",
    parameter = "vRR",
    shape1 = 500,
    shape2 = 500
  )
)

# Draw the Latin Hypercube (result is a matrix of uniform samples)
X <- randomLHS(1000, length(pp))
colnames(X) = names(pp)

# We then use the associated quantile function for each parameter to draw a value.  

lhc.draws.transformed <- cbind.data.frame(lapply(pp,function(x) 
{
  if (x[["type"]]=="beta")
  {
    qbeta(X[,x[["parameter"]]],shape1=x[["shape1"]],shape2=x[["shape2"]])
  } 
  else if (x[["type"]]=="uniform")
  {
    qunif(X[,x[["parameter"]]],min=x[["min"]],max=x[["max"]])
  }   
}
))

lhc.draws.transformed %>% head()
