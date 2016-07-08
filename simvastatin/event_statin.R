
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
  traj %>%
    set_attribute("aStartStatin", inputs$vHorizon*365+1) %>% # Don't assign again
    branch(
      function(attrs) attrs[["aStatinRxHx"]]+1,
      continue=rep(TRUE,2),
      create_trajectory() %>%
        set_attribute("aStatinRxHx", 1) %>% # Now they have a history of statin RX
        branch(
          function(attrs) 
          {
            # If not genotyped or wildtype gene, return 1 for simvastatin
            if(attrs[['aGenotyped_CVD']] != 1 || attrs[['aCVDgenotype']] == 1) return(1)
            
            # Otherwise, run probability of prescribing alternate
            sample(1:2, 1, prob=c(1-inputs$simvastatin$vProbSimvastatinAlt, inputs$simvastatin$vProbSimvastatinAlt))
          },  
          continue = rep(TRUE,2),
          create_trajectory("Simvastatin") %>%
            seize("simvastatin") %>%
            set_attribute("aCVDdrug", 1),
          create_trajectory("Alt. Simvastatin") %>%
            seize("alt_simvastatin") %>%
            set_attribute("aCVDdrug", 2)
        ),
      create_trajectory() %>% timeout(0) # Due to prior history, don't prescribe again
    )

}

statin <- function(traj, inputs)
{
  traj %>%
    statin_reactive_strategy(inputs) %>%
    assign_statin(inputs)
}
