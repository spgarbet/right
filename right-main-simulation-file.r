##################################################################################################################
# RIGHT - Simulation Model
# Note: Attributes Start With Prefix "a"
#
# git config --global user.name "graveja0"
# git config --global user.email "john.graves@vanderbilt.edu"
# git remote add origin git@github.com:graveja0/right-simulation.ig
# git push -u origin master

##################################################################################################################

####
## Set Directories and Load Packages
rm(list=ls())
setwd("~/Dropbox/Projects/right-simulation/")
pkg = list("simmer",
           "dplyr",
           "ggplot2",
           "reshape2",
           "tidyr")
invisible(lapply(pkg, library, character.only = TRUE))

#####
## Define Simulation Environment.
env  <- simmer("RIGHT-v1.0")

#####
## Define Simluation Scenario
inputs      <- list()
inputs$Scenario <- "PGx-Prospective"
inputs$vDAPT.SecondLine  <- "Ticagrelor"

#####
## Assign Initial Patient Attributes
epsilon = 0.000001
end.of.model = 36500
source("./simulation-files/initial-patient-attributes.r")
source("./simulation-files/PGx-attributes.r")
source("./simulation-files/dapt-indication.r")

assign_attributes <- function(traj, inputs)
{
  traj %>%
    assign_initial_attributes(inputs) %>%
    assign_dapt_attributes(inputs) %>%
    assign_CYP2C19_status(inputs) 

}

# These Need to Be Run After The Initial Even Times Have Been Determined 
assign_additional_attributes <- function(traj, inputs)
{
  traj %>%
    assign_PGx_attributes(inputs) 
}

####
## Secular Death
source('./simulation-files/event_secular_death.R')

####
## Event Registry
event_registry <- list(
  list(name          = "Secular Death",
       attr          = "eSecularTime",
       time_to_event = days_till_death,
       func          = secular_death),
  list(name          = "DAPT Initialized",
       attr          = "eDAPTInitialized",
       time_to_event = days_till_dapt,
       func          = dapt) 
)

#####
## Counters
counters <- c("Number Genotyped",
              "DAPT Initiated",
              "Switched.DAPT",
              "Secular Death",
              "Clopidogrel","Prasugrel","Ticagrelor","Received Aspirin","Aspirin",
              "aTimeInModel")

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
cleanup_on_death <- function(traj)
{
  traj %>% 
    #print_attrs() %>%
    release("aTimeInModel") %>%
      release("Aspirin") 
}


############################################################################################################################################################
####
## Seup and Run the Simulation.
source('./simulation-files/event_main_loop.R')

# Start the clock!
N <- 1000
ptm <- proc.time()
traj <- simulation(env, inputs)
env %>% create_counters(counters) %>%
   add_generator("patient", traj, at(rep(0, N)), mon=2) %>%
   run(36500) %>% # Simulate 100 years.
   wrap()
(timer = proc.time() - ptm)

# # Look at summary statistics
 arrivals <- get_mon_arrivals(env, per_resource = T)

# # Look at Initial Atttributes
  get.attributes = c("aGenotyped","aNumDAPT")
  see.attr = spread(plot_attributes(env)$data,key,value) 
#  head(see.attr[order(see.attr$name,see.attr$time),c("name","time",get.attributes)],n=10)
#  see.attr[see.attr$name==pp,c("name","time",get.attributes)]
 
 arrivals %>% count(resource)
 
 # Number by Group
 dapt.initiated  = within(arrivals[arrivals$resource=="DAPT Initiated",],{num_dapt = ave(replication,name,FUN=sum)}); hist(dapt.initiated$num_dapt)
 