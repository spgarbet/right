##################################################################################################################
# RIGHT - Simulation Model
# Note: Attributes Start With Prefix "a"
# To Do:
#     Annals paper has a cardiovascular mortality outcome for 1 year post PCI.  Should we add in?  Or just 
#       rely on the MACE outcome constructed elsewhere?
#     New Outcome: Periprocedural death due to PCI ? 0.12% (0.10-1.00%)
#     New Outcome: Periprocedural death due to CABG? 2.10% (1.0-10.0%)
##################################################################################################################

####
## 
# Set Directories and Load Packages
##
####

rm(list=ls())
#setwd("~/Dropbox/Projects/right-simulation/")
pkg = list("simmer",
           "dplyr",
           "ggplot2",
           "reshape2",
           "tidyr",
           "msm")
invisible(lapply(pkg, require, character.only = TRUE))



####
## 
# Define Simulation Environment.
#
# NOTE: This must be done at a global level for the simmer now() function to be available
#       inside trajectories. Without this at a global level, the simulation won't work.
####
env  <- simmer("RIGHT-v1.0")

####
## 
# Define Simulation Scenario
##
####
source("./inputs.R")

####
## Secular Death
source('./main/event_secular_death.R')

#####
## Clopidogrel
source("./clopidogrel/counters.R")
source("./clopidogrel/initial-patient-attributes.R")
source("./clopidogrel/cleanup.R")
source("./clopidogrel/PGx-attributes.r")
source("./clopidogrel/dapt-events.r")

####
## Simvastatin
source('./simvastatin/counters.R')
source('./simvastatin/simvastatin.R')
source('./simvastatin/cleanup.R')
source('./simvastatin/event_cvd.R')
source('./simvastatin/event_myopathy.R')

initialize_patient <- function(traj, inputs)
{
  traj %>%
    seize("n_patients")       %>%
    set_attribute("aGender",    function(attrs) sample(1:2,1,c(1-inputs$vPctFemale,inputs$vPctFemale))) %>% 
    set_attribute("aAge",       function(attrs) runif(1,inputs$vLowerAge,inputs$vUpperAge)) %>%
    set_attribute("aAgeInitial",function(attrs) attrs[['aAge']])  %>%
    assign_clopidogrel_attributes(inputs) %>%
    assign_simvastatin_attributes(inputs)
}

all_genotyped <- function(attrs)
{
   attrs[['aGenotyped_CVD']] == 1 && attrs[['aGenotyped_CYP2C19']] == 1
}

any_genotyped <- function(attrs)
{
   attrs[['aGenotyped_CVD']] == 1 || attrs[['aGenotyped_CYP2C19']] == 1
}

panel_test <- function(traj, inputs)
{
  # Cannot put logic here to check for already done
  # because premptive strategy may have every predict succeed
  # and it calls this without knowing that it effectively did 
  # a full panel
  genotype_clopidogrel(inputs) %>%
  genotype_simvastatin(inputs) %>%
  mark("panel_test")
}

# Must Be Run After The Initial Event Times Have Been Determined 
# For predict to work
preemptive_strategy <- function(traj, inputs)
{

  # Note this doesn't have to use branch, because it's a global that every trajectory gets
  if        (inputs$vPreemptive == "None"     )
  {
    traj # Do nothing
  } else if (inputs$vPreemptive == "Panel"    )
  {
    traj %>% panel_test(inputs)
  } else if (inputs$vPreemptive == "PREDICT"  )
  {
    traj %>%
      predict_clopidogrel(inputs) %>%
      predict_simvastatin(inputs) %>%
      branch(
        function(attrs) any_genotyped(attrs),
        continue=rep(TRUE,2),
        create_trajectory() %>% timeout(0), # Nothing genotyped, do nothing
        create_trajectory() %>% panel_test(inputs) # Something was genotyped via PREDICT, do panel
      )
  } else if (inputs$vPreemptive == "Age >= 50")
  {
    traj %>%
    branch(
      function(attrs) if(attrs[['aAge']] >= 50) 1 else 2,
      continue = c(TRUE, TRUE),
      create_trajectory() %>% timeout(0), # Do nothing
      create_trajectory() %>% panel_test(inputs)
    )
  } else stop("Unhandled Preemptive Strategy")
}

####
## Cleanup 
cleanup_on_termination <- function(traj)
{
  traj %>% 
    #print_attrs() %>%
    release("n_patients") %>%
    cleanup_clopidogrel() %>%
    cleanup_simvastatin() 
}

terminate_simulation <- function(traj)
{
  traj %>%
  branch(
    function() 1, 
    continue=FALSE,
    create_trajectory() %>% cleanup_on_termination()
  )
}

####
## Event Registry
event_registry <- list(
  
  #### Global Events
  list(name          = "Secular Death",
       attr          = "aSecularDeathTime",
       time_to_event = days_till_death,
       func          = secular_death),
  list(name          = "Terminate at 10 years",
       attr          = "aTerminate",
       time_to_event = function(attrs) 365.0*inputs$vHorizon,
       func          = terminate_simulation),
  
  #### Clopidogrel Events
  list(name          = "DAPT Initialized",
       attr          = "aTimeDAPTInitialized",
       time_to_event = days_till_dapt,
       func          = dapt) ,
  list(name          = "DAPT Ended",
       attr          = "aDAPTEnded",
       time_to_event = dapt_end_time,
       func          = dapt_end),
  list(name          = "Stent Thromb",
       attr          = "aST",
       time_to_event = time_to_ST,
       func          = ST_event),
  list(name          = "Myocardial Infarction",
       attr          = "aMI",
       time_to_event = time_to_MI,
       func          = MI_event) , 
  list(name          = "Revascularization",
       attr          = "aRV",
       time_to_event = time_to_RV,
       func          = RV_event) ,
  list(name          = "Extracranial TIMI Non-Fatal",
       attr          = "aExtBleed",
       time_to_event = time_to_ExtBleed,
       func          = ExtBleed_event) ,
  list(name          = "Intracranial TIMI Major Nonfatal",
       attr          = "aIntBleed",
       time_to_event = time_to_IntBleed,
       func          = IntBleed_event),
  list(name          = "TIMI Minor",
       attr          = "aTIMIMinor",
       time_to_event = time_to_TIMIMinor,
       func          = TIMIMinor_event) ,
  list(name          = "Fatal Bleed",
       attr          = "aFatalBleed",
       time_to_event = time_to_FatalBleed,
       func          = FatalBleed_event),
  
  #### Simvastatin Events
  list(name          = "Mild Myopathy",
       attr          = "aMildMyoTime",
       time_to_event = days_till_mild_myopathy,
       func          = mild_myopathy),
  list(name          = "Moderate Myopathy",
       attr          = "aModMyoTime",
       time_to_event = days_till_mod_myopathy,
       func          = mod_myopathy),
  list(name          = "Severe Myopathy",
       attr          = "aSevMyoTime",
       time_to_event = days_till_sev_myopathy,
       func          = sev_myopathy),
  list(name          = "Cardiovascular Disease",
       attr          = "aCVDTime",
       time_to_event = days_till_cvd,
       func          = cvd),
  list(name          = "Reassess CVD Risk",
       attr          = "aCVDReassess",
       time_to_event = days_till_reassess_cvd,
       func          = reassess_cvd)
)

#####
## Counters
source("./main/counters.R")
counters <- c(counters.gen, counters.dapt, counters.simvastatin)


# Note that simply releasing a resource will release it for everyone.  Therefore, we need
# to have a branch that only releases it for folks with, for example, a drug.  

############################################################################################################################################################
####
##
# Setup and Run the Simulation.
##
####
source('./main/event_main_loop.R')


############################################################
set.seed(12345)

ptm <- proc.time()
traj <- simulation(env, inputs)
env %>% create_counters(counters)

env %>%
  add_generator("patient", traj, at(rep(0, inputs$vN)), mon=2) %>%
  run(365*inputs$vHorizon+10) %>% # Simulate just past horizon
  wrap()
(timer = proc.time() - ptm)
############################################################

####
##
# Count Number of Events
##
####
arrivals <- get_mon_arrivals(env, per_resource = T)
arrivals %>% count(resource) 

