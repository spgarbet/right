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
source("./model-inputs-main.r")

#####
## Assign Initial Patient Attributes

source("./clopidogrel/initial-patient-attributes.r")
source("./clopidogrel/PGx-attributes.r")
source("./clopidogrel/dapt-events.r")

assign_attributes <- function(traj, inputs)
{
  traj %>%
    assign_initial_clopidogrel_attributes(inputs) %>%
    assign_dapt_attributes(inputs) %>%
    assign_CYP2C19_status(inputs) 
}

# These Need to Be Run After The Initial Event Times Have Been Determined 
assign_additional_attributes <- function(traj, inputs)
{
  traj %>%
    assign_clopidogrel_PGx_attributes(inputs) 
}

####
## Secular Death
source('./main/event_secular_death.R')

####
## Event Registry
event_registry <- list(
  list(name          = "Secular Death",
       attr          = "aSecularDeathTime",
       time_to_event = days_till_death,
       func          = secular_death),
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
       func          = FatalBleed_event)  
)

#####
## Counters
source("./main/counters.r")

exec <- function(traj, func)
{
  traj %>%
  timeout(function(attrs) {func(attrs); 0})
}

print_attrs <- function(traj)
{
  exec(traj, function(attrs) print(attrs))
}

####
## Cleanup 
cleanup_on_death <- function(traj,attrs)
{
  traj %>% 
    #print_attrs() %>%
    release("n_patients") %>%
      branch(
        function(attrs) ifelse(attrs[["aAspirin"]]==1,1,2),
        continue = c(TRUE,TRUE),
        create_trajectory() %>% release("aspirin"),
        create_trajectory() %>% timeout(0)
      )
}

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
  run(365*inputs$vHorizon) %>% # Simulate 100 years.
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


# Gx.Status = spread(attributes %>% group_by(name,key) %>% summarize(last = last(value)),key,last) %>%  select(name,aCYP2C19,aGenotyped) %>% ungroup() %>% 
#   mutate(aCYP2C19=factor(aCYP2C19),
#          LOF = as.numeric( aCYP2C19==1) )
# arrivals %>% inner_join(Gx.Status) %>% group_by(LOF) %>%  count(resource) %>% filter(resource=="Stent Thrombosis")
# 
# ####
# ## 
# # Get Data Files With Patient Attributes
# ##
# ####
# 
# attributes <- arrange(get_mon_attributes(env),name,key,time)
# first.attributes <- spread(attributes %>% group_by(name,key) %>% summarize(first = first(value)),key,first)
# last.attributes <- spread(attributes %>% group_by(name,key) %>% summarize(last = last(value)),key,last) 
# all.attributes <- spread(attributes %>% group_by(name,key,time) %>% summarize(first = mean(value)),key,first)
# 
# 
