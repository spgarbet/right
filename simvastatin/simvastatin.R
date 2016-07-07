  ###########################################################################
 ##
## Health Policy Cost Effectiveness Simulation
##
## This is a modifiable simulator, that allows for estimation of costs and
## quality adjusted life years for patients undergoing various health events.
## 
## The event loop is abstracted. Each tick of the "now()" is a day. All
## internal units for the simulation are days.
##
## There is an event_registry where users can add events in a straight-
## forward manner. Some knowledge of simmer is still required, but the
## intricacies of the main event loop are dealt with.
##
## This leaves the top half of this example file as a modifiable
## section, and the bottom half as event loop code that hopefully 
## does not require modification. If it does, it should be folded
## back into the master example file.
## Please email <shawn.garbett@vanderbilt.edu> if this is required.
## 
## An event which stops the simulation must be included in the event
## list. Typically this would be a secular death event.
##
## MODIFY THIS FOR YOUR SIMULATION
##

library(simmer)

# Define simulation environment first
# IMPORTANT: this allows calls of now() inside trajectories
env  <- simmer("Simvastatin")

  ##############################################
 ##
## User defined counters / trackers
##
## MODIFY THIS LIST FOR YOUR SIMULATION
##
counters <- c("secular_death",
              "mild_myopathy",
              "mod_myopathy",
              "sev_myopathy",
              "rahbdo_death",
              "cvd",
              "cvd_death",
              "stopped",
              "switched",
              "drug1",
              "drug2",
              "drug3",
              "drug4",
              "life",
              "genotyped")

  ##############################################
 ##
## User defined events section
##
## An event which stops the simulation must be
## included in the event list. Typically this
## would be a secular death event.
##
## MODIFY THIS FOR YOUR SIMULATION
##

stop_treatment <- function(traj)
{
  traj %>% branch(
    function(attrs) attrs[["CVDdrug"]]+1,
    merge=rep(TRUE,5),
    create_trajectory() %>% timeout(0),
    create_trajectory() %>% release("drug1"),
    create_trajectory() %>% release("drug2"),
    create_trajectory() %>% release("drug3"),
    create_trajectory() %>% release("drug4")
  ) %>%
  set_attribute("CVDdrug", 0)
}

# Cleanup a death function, called for any form of death
# This is needed for use in any event that results in a
# death. One must closeout "in use" counters, otherwise they won't
# appear in statistics
cleanup_on_death <- function(traj)
{
  traj %>% 
  release("life") %>%
  stop_treatment()
}

source('event_secular_death.R')
source('event_myopathy.R')
source('event_cvd.R')

# Main event registry that is used by the event loop to create and track
# event in a generic manner. Each has a:
#   * name:          How you want it to appear in any printouts
#   * attr:          How it will be stored in the patient (trajectory) attributes
#   * time_to_event: a function given a list of attributes, returns in days
#                    how long till the next event. NOTE: The variable in
#                    must be named "attrs"
#   * func:          a function that given a trajectory, modifies it and
#                    returns the modified trajectory.
#
event_registry <- list(
  list(name          = "Secular Death",
       attr          = "eSecularTime",
       time_to_event = days_till_death,
       func          = secular_death),
  list(name          = "Mild Myopathy",
       attr          = "eMildMyoTime",
       time_to_event = days_till_mild_myopathy,
       func          = mild_myopathy),
  list(name          = "Moderate Myopathy",
       attr          = "eModMyoTime",
       time_to_event = days_till_mod_myopathy,
       func          = mod_myopathy),
  list(name          = "Severe Myopathy",
       attr          = "eSevMyoTime",
       time_to_event = days_till_sev_myopathy,
       func          = sev_myopathy),
  list(name          = "Cardiovascular Disease",
       attr          = "eCVDTime",
       time_to_event = days_till_cvd,
       func          = cvd),
  list(name          = "Reassess CVD Risk",
       attr          = "eCVDReassess",
       time_to_event = days_till_reassess_cvd,
       func          = reassess_cvd)
)


  #################################################
 ##
## Assign patient attributes utilized in simulation
##
## MODIFY THIS FOR YOUR SIMULATION
##
##
assign_gender_and_age <- function(traj, inputs)
{
  # Assign Gender and Age based on Gender
  traj %>%
  seize("life") %>%
  branch(
    function() sample(1:2, 1, prob=c(0.5, 0.5)),
    merge=c(TRUE,TRUE),
    create_trajectory("male") %>%
      set_attribute("gender", 1)                      %>%
      set_attribute("ageAtStart", function() inputs$vAge),
    create_trajectory("female") %>%
      set_attribute("gender", 2)                      %>%
      set_attribute("ageAtStart", function() inputs$vAge)
  ) %>%
  set_attribute("age", function(attrs) attrs[['ageAtStart']])
}

assign_cvd_genotype <- function(traj, inputs)
{
  # Assign Genotype
  traj %>% branch(
    function() sample(1:3, 1, prob=c(0.730, 0.249, 0.021)),
    merge=rep(TRUE,3),
    create_trajectory("TT") %>% set_attribute("CVDgenotype", 1),
    create_trajectory("TC") %>% set_attribute("CVDgenotype", 2),
    create_trajectory("CC") %>% set_attribute("CVDgenotype", 3)
  )
}

assign_cvd_medication <- function(traj, inputs)
{
  traj %>%
  set_attribute("second_line",
    function() {
    if(inputs$vSecondLine == "Atorvastin")          {return(2)} else
    if(inputs$vSecondLine == "Rosuvastatin")        {return(3)} else
    if(inputs$vSecondLine == "Low/Mod Dose Statin") {return(4)}
    
    # Something went very wrong
    stop("Invalid Logic in assigning cvd medication")
  }) %>%
  branch(
    function() (is.na(inputs$vPGx) || inputs$vPG=="None")  + 1,
    merge=c(TRUE,TRUE),
    create_trajectory() %>% timeout(0),
    create_trajectory("Genotyped") %>% mark("genotyped")
  ) %>%
  branch(
    function(attrs) {
      # No treatment at all
      if(!inputs$vTX) return(5)
      
      # If there is no genotyping, or it's reactive, default to Simvastatin
      if(is.na(inputs$vPGx) || inputs$vPGx == "Reactive") return(1)
      
      # If the genotype is good metabolizer, then default to Simvastatin
      if(attrs[['CVDgenotype']] == 1) return(1)
      
      # Assign second line
      return(attrs[['second_line']])
    },
    merge=rep(TRUE,5),
    create_trajectory("Simvastatin")  %>%
      set_attribute("CVDdrug", 1) %>%
      seize("drug1"),
    create_trajectory("Atorvastin")   %>%
      set_attribute("CVDdrug", 2) %>%
      seize("drug2") %>%
      mark("switched"),
    create_trajectory("Rosuvastatin") %>%
      set_attribute("CVDdrug", 3) %>%
      seize("drug3") %>%
      mark("switched"),
    create_trajectory("Low/Moderate Dose Statin") %>%
      set_attribute("CVDdrug", 4) %>%
      seize("drug4") %>%
      mark("switched"),
    create_trajectory("No Treatment") %>%
      set_attribute("CVDdrug", 0)
  )
}

## This function is referenced in main event loop
assign_attributes <- function(traj, inputs)
{
  # inputs$vTX  : boolean to treat or not to treat
  # inputs$vPGx : NA, "Preemptive", or "Reactive"
  # inputs$vSecondLine : "Atorvastin", "Rosuvastatin", or "Low/Mod Dose Statin"
  
  traj %>%
  assign_gender_and_age(inputs) %>%
    #timeout(function(attrs) {print("Assign Attrs"); print(attrs); 0}) %>%
  assign_cvd_genotype(inputs)   %>%
    #timeout(function(attrs) {print("Assign Attrs"); print(attrs); 0}) %>%
  assign_cvd_medication(inputs) #%>% timeout(function(attrs) {print("Assign Attrs"); print(attrs); 0})
}

source('event_main_loop.R')

annual_discount_rate <- 0.03
cont_discount_rate   <- -log(1-annual_discount_rate) # Yearly Time Scale
discounted_cost <- function(start_day, end_day, base_yearly_cost, rate = cont_discount_rate)
{
  base_yearly_cost*(exp(-rate*start_day/365) - exp(-rate*end_day/365))/rate 
}

simvastatin <- function(inputs, N=14000)
{
  reactive({
    env  <- simmer("Simvastatin") %>% 
      create_counters(counters)

    traj  <- simulation(env, inputs)

    env %>%
      add_generator("patient", traj, at(rep(0, N)), mon=2) %>%
      run(36500) %>% # Simulate 100 years.
      wrap()
  
    arrivals <- get_mon_arrivals(env, per_resource = T)

    arrivals$resource <- factor(arrivals$resource, counters)
  
    # 1 day events
    events_to_fix_end <- c("mild_myopathy", "genotyped")
    arrivals[arrivals$resource %in% events_to_fix_end,]$end_time <- arrivals[arrivals$resource %in% events_to_fix_end,]$start_time + 1.0
  
    # 30 day events
    events_to_fix_end <- c("mod_myopathy","sev_myopathy", "cvd")
    arrivals[arrivals$resource %in% events_to_fix_end,]$end_time <- arrivals[arrivals$resource %in% events_to_fix_end,]$start_time + 1.0
  
    # Compute total activity times
    arrivals$activity_time <- arrivals$end_time - arrivals$start_time
  
    # Computes discounted rate of time
    arrivals$discounted_time <- discounted_cost(arrivals$start_time, arrivals$end_time, 365.0)
  
    # Compute Event base costs
    idx <- function(str) {as.numeric(factor(str, levels=levels(arrivals$resource)))}
    base_cost_map <- reactive({
      base_cost_map <- rep(0, nlevels(arrivals$resource))
      base_cost_map[idx("drug1")]         <- inputs$vCostDrug1/365
      base_cost_map[idx("drug2")]         <- inputs$vCostDrug2/365
      base_cost_map[idx("drug3")]         <- inputs$vCostDrug3/365
      base_cost_map[idx("drug4")]         <- inputs$vCostDrug4/365
      base_cost_map[idx("genotyped")]     <- inputs$vCostPGx
      base_cost_map[idx("mild_myopathy")] <-   129
      base_cost_map[idx("mod_myopathy")]  <-  2255/30
      base_cost_map[idx("sev_myopathy")]  <- 12811/30
      base_cost_map[idx("cvd")]           <- 20347/30
    })
  
    # Compute Disutility costs
    base_disutility_map <- rep(0, nlevels(arrivals$resource))
    base_disutility_map[idx("mild_myopathy")] <- 0.01
    base_disutility_map[idx("mod_myopathy")]  <- 0.05
    base_disutility_map[idx("sev_myopathy")]  <- 0.53
    base_disutility_map[idx("cvd")]           <- 0.2445
  
    arrivals$discounted_cost <- arrivals$discounted_time*base_cost_map[as.numeric(arrivals$resource)]
    arrivals$disutility <- arrivals$discounted_time*base_disutility_map[as.numeric(arrivals$resource)]
  
    arrivals
  })
}




