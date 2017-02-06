
rm(list=ls())

pkg = list("simmer",
           "ggplot2",
           "reshape2",
           "plyr", #need to load this before "dplyr"
           "tidyr",
           "dplyr",
           "msm",
           "data.table",
           "deSolve")
invisible(lapply(pkg, require, character.only = TRUE))


###initial inputs
epsilon <- 0.000000000001
inputs <- list(
  vAge = 40,
  
  vRiskA = 0.1,
  vDurationA = 5,
  vRR = 0.95,
  vFatalA = 0.05,
  
  vRiskB = 0.5,
  vDurationB = 5,
  
  vStrategy = "Treat", # "Standard" or "Treat"
  vHorizon  = 10,
  vN = 100,
  
  disutilities = list(
    A_survive = 0.25,
    A_death = 1,
    B = 0.1
  ),
  
  durations = list(
    B = 365
  ),
  
  type = list(
    A_survive = 0,
    A_death = 0,
    B = 1
  ),
  
  costs = list(
    A_survive = 10000,
    A_death = 10000,
    B = 5000/365, #daily cost
    treat=0
  )
)



###
###assign attributes
initialize_patient <- function(traj, inputs)
{
  traj %>%
    seize("time_in_model") %>%
    set_attribute("aAgeInitial", function() inputs$vAge) %>%
    set_attribute("aAge", function(attrs) attrs[['aAgeInitial']]) %>%
    set_attribute("aRR_A", 1) %>%
    set_attribute("aRR_B", epsilon) %>%
    set_attribute("aTreat", function() ifelse(inputs$vStrategy=="Treat",1,2)) %>%
    branch(
      function(attrs) attrs[['aTreat']],
      continue = c(TRUE, TRUE),
      trajectory("Treat") %>% mark("treat"),
      trajectory("Standard") %>% timeout(0)
    )
}

########
#events
source('./events_simple.R')
terminate_simulation <- function(traj, inputs)
{
  traj %>%
    branch(
      function() 1, 
      continue=FALSE,
      trajectory() %>% release("time_in_model") 
    )
}

###
#fill in event_registry
event_registry <- list(

  list(name          = "Event A",
       attr          = "attA",
       time_to_event = days_till_A,
       func          = event_A,
       reactive      = FALSE),
  list(name          = "Event B",
       attr          = "attB",
       time_to_event = days_till_B,
       func          = event_B,
       reactive      = FALSE),
  list(name          = "Terminate at time horizonb",
       attr          = "aTerminate",
       time_to_event = function(attrs,inputs) 365.0*inputs$vHorizon,
       func          = terminate_simulation,
       reactive      = FALSE)
)

#### Counters
counters <- c(
  "time_in_model", 
  "A_death",
  "A",
  "A_survive",
  "B",
  "treat"
)

source('./event_main_loop_simple.R')


##########
# Start the clock!
exec.simulation <- function(inputs)
{
  set.seed(12345)
  env  <<- simmer("Simple")
  traj <- simulation(env, inputs)
  env %>% create_counters(counters)
  
  env %>%
    add_generator("patient", traj, at(rep(0, inputs$vN)), mon=2) %>%
    run(365*inputs$vHorizon+1) %>% # Simulate just past horizon
    wrap()
  
  get_mon_arrivals(env, per_resource = T)
}


