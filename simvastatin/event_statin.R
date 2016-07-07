
####
## Assign Time to Simvastatin therapy
days_till_statin <- function(attrs, inputs) 
{
  rweibull(1, inputs$simvastatin$vShape, inputs$simvastatin$vScale)
}

statin <- function(traj, inputs)
{
  traj
}
