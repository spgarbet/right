days_till_warfarin <- function(attrs, inputs)
{
  on = attrs[["aOnWarfarin"]]
  if (inputs$vDrugs$vWarfarin == TRUE & on==2)
    {t2e <- rweibull(1,inputs$warfarin$vshape_timetowarfarin,inputs$warfarin$vscale_timetowarfarin)} 
  else {t2e <- inputs$vHorizon*365+1}
  return(t2e)
} 

#for all genotyped patients through preemptive strategies (Panel or PREDICT), physician can choose to use or ignore the test results
#under reactive strategies, physician can also choose to order test for those not genotyped
warfarin_reactive_strategy <- function(traj, inputs)
{
  if(inputs$vReactive == "None") 
  {
    traj %>%
      branch(
        function(attrs) attrs[['aGenotyped_Warfarin']],
        continue=c(TRUE, TRUE),
        create_trajectory("have test results") %>%  
          branch(
            function(attrs) sample(1:2,1,prob=c(inputs$warfarin$vProbabilityRead, 1-inputs$warfarin$vProbabilityRead)),
            continue=c(TRUE,TRUE),
            create_trajectory() %>% timeout(0), #physician reads and utilizes test results
            create_trajectory() %>% set_attribute("aGenotyped_Warfarin", 2) #ignore test results, treat as non-genotyped
          ),
        create_trajectory("not have") %>% timeout(0)
      )
  } else if (inputs$vReactive == "Single")
  {
    traj %>%
      branch(
        function(attrs) attrs[['aGenotyped_Warfarin']],
        continue=c(TRUE, TRUE),
        create_trajectory("have test results") %>%  
          branch(
            function(attrs) sample(1:2,1,prob=c(inputs$warfarin$vProbabilityRead, 1-inputs$warfarin$vProbabilityRead)),
            continue=c(TRUE,TRUE),
            create_trajectory() %>% timeout(0), #physician reads and utilizes test results
            create_trajectory() %>% set_attribute("aGenotyped_Warfarin", 2) #ignore test results, treat as non-genotyped
          ),
        create_trajectory("not have") %>% 
          branch(
            function(attrs) sample(1:2,1,prob=c(1- inputs$warfarin$vProbabilityReactive,  inputs$warfarin$vProbabilityReactive)),
            continue=c(TRUE,TRUE),
            create_trajectory() %>% timeout(0),
            create_trajectory() %>% set_attribute("aGenotyped_Warfarin", 1) %>% mark("single_test")
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
            function(attrs) sample(1:2,1,prob=c(1- inputs$warfarin$vProbabilityReactive,  inputs$warfarin$vProbabilityReactive)),
            continue=c(TRUE,TRUE),
            create_trajectory() %>% timeout(0),
            create_trajectory() %>% panel_test()
          ), # Not all genotyped, then do it
        create_trajectory("panel tested") %>% 
          branch(
            function(attrs) sample(1:2,1,prob=c(inputs$warfarin$vProbabilityRead, 1-inputs$warfarin$vProbabilityRead)),
            continue=c(TRUE,TRUE),
            create_trajectory() %>% timeout(0), #physician reads and utilizes test results
            create_trajectory() %>% set_attribute("aGenotyped_Warfarin", 2) #ignore test results, treat as non-genotyped
          ) 
      )
  } else stop("Unhandled Reactive Warfarin Strategy")
}

initial_INR <- function(x) 
{
  set.seed(x) 
  sample(inputs$warfarin$vINRvalue, 1, prob=inputs$warfarin$vINRfreq)
}

assign_indication <- function(x) 
{
  set.seed(x) 
  sample(1:2, 1, prob=c(inputs$warfarin$vpct_afib, 1-inputs$warfarin$vpct_afib))
}


INR_status <- function(x) {
  if (x>=2 & x<= 3) {return(1)} 
  else {return(2)}
}  

start_warfarin <- function(traj, inputs)
{
  traj %>%
    seize("warfarin") %>% 
    set_attribute("aTimeToStartWarfarin", inputs$vHorizon*365+1) %>% # cannot trigger "start warfarin" again
    set_attribute("sWarfarinEvents", 1) %>%  #switch on warfarin events
    set_attribute("aOnWarfarin", 1) %>% # start on warfarin 
    set_attribute("sINRMonitor", 1) %>% # start monitoring INR in 90 days
    
    #assign indication
    set_attribute("aWarfarinIndication", function(attrs) assign_indication(attrs[["aSeed"]])) %>%
    
    #assign initial INR
    set_attribute("aINRInitial", function(attrs) initial_INR(attrs[["aSeed"]])) %>%
    set_attribute("aINR", function(attrs) attrs[["aINRInitial"]]) %>%
    set_attribute("aInRange", function(attrs) INR_status(attrs[["aINRInitial"]])) %>%
    
    #mark initial INR cat 
    branch(
      function(attrs) attrs[["aInRange"]],
      continue=rep(TRUE, 2),
      create_trajectory("Initial In Range") %>% mark("Initial_InRange") %>%
        seize("in_range"),
      create_trajectory("Initial Out of Range") %>% mark("Initial_OutRange") %>%
        seize("out_of_range") 
    )
}

prescribe_warfarin <- function(traj, inputs)
{
  traj %>% 
    mark("warfarin_start") %>% 
    warfarin_reactive_strategy(inputs) %>%
    start_warfarin(inputs) %>%
    #downstream events
    adj_clock()
}


