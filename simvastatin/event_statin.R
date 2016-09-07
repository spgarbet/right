
####
## Assign Time to Simvastatin therapy
days_till_statin <- function(attrs, inputs) 
{
  if (inputs$vDrugs$vSimvastatin & attrs[['aStatinRxHx']]==0)
  {
    rweibull(1, inputs$simvastatin$vShape, inputs$simvastatin$vScale)
  } else 
  {
    inputs$vHorizon*365+1
  }
}

statin_read_test <- function(traj, inputs)
{
  traj %>%
    branch(
      function(attrs) attrs[['aGenotyped_CVD']],
      continue=c(TRUE, TRUE),
      #for all genotyped patients through preemptive strategies (Panel or PREDICT), physician can choose to use or ignore the test results
      create_trajectory("have test results") %>%  
        branch(
          function(attrs) sample(1:2,1,prob=c(inputs$simvastatin$vProbabilityRead, 1-inputs$simvastatin$vProbabilityRead)),
          continue=c(TRUE,TRUE),
          create_trajectory() %>% timeout(0), #physician reads and utilizes test results
          create_trajectory() %>% set_attribute("aGenotyped_CVD", 2) %>% mark("ignore_test") #ignore test results, treat as non-genotyped
        ),
      create_trajectory("not have") %>% timeout(0)
    )
}  

#for all genotyped patients through preemptive strategies (Panel or PREDICT), physician can choose to use or ignore the test results
#under reactive strategies, physician can also choose to order test for those not genotyped
statin_reactive_strategy <- function(traj, inputs)
{
  if(inputs$vReactive == "None") 
  {
    traj %>%
      branch(
        function(attrs) attrs[['aGenotyped_CVD']],
        continue=c(TRUE, TRUE),
        create_trajectory("have test results") %>%  
          branch(
            function(attrs) sample(1:2,1,prob=c(inputs$simvastatin$vProbabilityRead, 1-inputs$simvastatin$vProbabilityRead)),
            continue=c(TRUE,TRUE),
            create_trajectory() %>% timeout(0), #physician reads and utilizes test results
            create_trajectory() %>% set_attribute("aGenotyped_CVD", 2) %>% mark("ignore_test") #ignore test results, treat as non-genotyped
          ),
        create_trajectory("not have") %>% timeout(0)
      )
  } else if (inputs$vReactive == "Single")
  {
    traj %>%
      branch(
        function(attrs) attrs[['aGenotyped_CVD']],
        continue=c(TRUE, TRUE),
        create_trajectory("have test results") %>%  
          branch(
            function(attrs) sample(1:2,1,prob=c(inputs$simvastatin$vProbabilityRead, 1-inputs$simvastatin$vProbabilityRead)),
            continue=c(TRUE,TRUE),
            create_trajectory() %>% timeout(0), #physician reads and utilizes test results
            create_trajectory() %>% set_attribute("aGenotyped_CVD", 2) %>% mark("ignore_test") #ignore test results, treat as non-genotyped
          ),
        create_trajectory("not have") %>% 
          branch(
            function(attrs) sample(1:2,1,prob=c(1- inputs$simvastatin$vProbabilityReactive,  inputs$simvastatin$vProbabilityReactive)),
              continue=c(TRUE,TRUE),
              create_trajectory() %>% timeout(0),
              create_trajectory() %>% set_attribute("aGenotyped_CVD", 1) %>% mark("single_test")
              )
      )
  } else if (inputs$vReactive == "Panel")
  {
    traj %>%
    branch(
      function(attrs) all_genotyped(attrs)+1,
      continue=c(TRUE, TRUE),
      create_trajectory("not panel tested") %>% 
        branch(
          function(attrs) sample(1:2,1,prob=c(1- inputs$simvastatin$vProbabilityReactive,  inputs$simvastatin$vProbabilityReactive)),
          continue=c(TRUE,TRUE),
          create_trajectory() %>% timeout(0),
          create_trajectory() %>% panel_test()
        ), # Not all genotyped, then do it
      create_trajectory("panel tested") %>% 
        branch(
          function(attrs) sample(1:2,1,prob=c(inputs$simvastatin$vProbabilityRead, 1-inputs$simvastatin$vProbabilityRead)),
            continue=c(TRUE,TRUE),
            create_trajectory() %>% timeout(0), #physician reads and utilizes test results
            create_trajectory() %>% set_attribute("aGenotyped_CVD", 2) %>% mark("ignore_test") #ignore test results, treat as non-genotyped
        ) 
    )
  } else stop("Unhandled Reactive Statin Strategy")
}

assign_statin <- function(traj, inputs)
{
  traj %>%
    branch(
      function(attrs) min(attrs[["aStatinRxHx"]]+1, 2), # 0 = No history, 1+ prior history
      continue=c(TRUE,TRUE),
      create_trajectory() %>%
        mark("statin_any") %>% 
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
        ) %>% 
        branch(
          function(attrs)
          {
            if (attrs[['aCVDdrug']]!=1 & attrs[['aGenotyped_CVD']] == 1) return(1)
            return(2)
          },
          continue = c(TRUE,TRUE),
          create_trajectory() %>% mark("statin_switched_PGx"),
          create_trajectory() %>% timeout(0)
        ),
      create_trajectory() %>% timeout(0) # Due to prior history, don't do anything, as this has already been dealt with
    )
}

prescribe_statin <- function(traj, inputs)
{
  traj %>%
    statin_reactive_strategy(inputs) %>%
    assign_statin(inputs)
}
