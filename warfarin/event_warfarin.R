days_till_warfarin <- function(attrs, inputs)
{
  on = attrs[["aOnWarfarin"]]
  if (inputs$vDrugs$vWarfarin == TRUE & on==2)
    {t2e <- min(c(attrs[["aTimetoWarfarin_AF"]],attrs[["aTimetoWarfarin_NonAF"]]))} 
  else {t2e <- inputs$vHorizon*365+1}
  return(t2e)
} 

warfarin_reactive_strategy <- function(traj, inputs)
{
  if(inputs$vReactive == "None") 
  {
    traj # Do nothing to trajectory
  } else if (inputs$vReactive == "Single")
  {
    traj %>%
      branch(
        function(attrs) attrs[['aGenotyped_Warfarin']],
        continue=c(TRUE, TRUE),
        create_trajectory() %>% timeout(0),
        create_trajectory() %>% set_attribute("aGenotyped_Warfarin", 1) %>% mark("single_test")
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

INR_status <- function(x) {
  if (x>=2 & x<= 3) {return(1)} 
  else {return(2)}
}  

assign_inital_INR <- function() sample(inputs$warfarin$vINRvalue, 1, prob=inputs$warfarin$vINRfreq)

start_warfarin <- function(traj, inputs)
{
  traj %>%
    seize("warfarin") %>% 
    set_attribute("aTimeToStartWarfarin", inputs$vHorizon*365+1) %>% # cannot trigger "start warfarin" again
    set_attribute("sWarfarinEvents", 1) %>%  #switch on warfarin events
    set_attribute("aOnWarfarin", 1) %>% # start on warfarin 
    set_attribute("sINRMonitor", 1) %>% # start monitoring INR in 90 days
    #assign initial INR
    set_attribute("aINRInitial", function() assign_inital_INR()) %>%
    set_attribute("aINR", function(attrs) attrs[["aINRInitial"]]) %>%
    #mark initial INR cat 
    branch(
      function(attrs) INR_status(attrs[["aINRInitial"]]),
      continue=rep(TRUE, 2),
      create_trajectory("Initial In Range") %>% 
        seize("in_range") %>%
        set_attribute("aInRange", 1),
      create_trajectory("Initial Out of Range") %>% 
        seize("out_of_range") %>%
        set_attribute("aInRange", 2)
    )
}

warfarin <- function(traj, inputs)
{
  traj %>% 
    mark("warfarin_start") %>% 
    warfarin_reactive_strategy(inputs) %>%
    start_warfarin(inputs) %>%
    #downstream events
    adj_clock()
}


