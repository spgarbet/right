
####
## Assign Time to Simvastatin therapy
days_till_statin <- function(attrs, inputs) 
{
  rweibull(1, inputs$simvastatin$vShape, inputs$simvastatin$vScale)
}

statin_reactive_strategy <- function(traj, inputs)
{
  if(inputs$vReactive == "None") 
  {
    traj # Do nothing to trajectory
  } else if (inputs$vReactive == "Single")
  {
    traj %>%
    branch(
      function(attrs) attrs[['aGenotyped_CVD']],
      continue=c(TRUE, TRUE),
      create_trajectory() %>% timeout(0),
      create_trajectory() %>% set_attribute("aGenotyped_CVD", 1) %>% mark("single_test")
    )
  } else if (inputs$vReactive == "Panel")
  {
    traj %>%
    branch(
      function(attrs) all_genotyped(attrs)+1,
      continue=c(TRUE, TRUE),
      create_trajectory() %>% panel_test(), # Not all genotyped, then do it
      create_trajectory() %>% timeout(0)    # Already done, ignore
    )
  } else stop("Unhandled Reactive Statin Strategy")
}

assign_statin <- function(traj, inputs)
{
  traj
}

statin <- function(traj, inputs)
{
  traj %>%
    statin_reactive_strategy(inputs) %>%
    assign_statin(inputs)
}
