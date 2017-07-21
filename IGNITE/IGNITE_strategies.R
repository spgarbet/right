pkg = list("simmer",
           "data.table",
           "plyr",
           "dplyr",
           "tidyr",
           "reshape2",
           "ggplot2",
           "downloader",
           "msm",
           "quantmod")
invisible(lapply(pkg, require, character.only = TRUE))
rm=list(ls())
setwd("/Users/zilu/Box Sync/IGNITE")
#setwd("/Users/ziluzhou1/Box Sync/IGNITE")
#setwd("/Users/zilu/Desktop/right-simulation/IGNITE")
source("./run_IGNITE.r")

###Single Drug 
inputs$vDrugs = list(vSimvastatin = F, 
                     vWarfarin = F,
                     vClopidogrel = T)

#inputs$warfarin$vscale_timetowarfarin <- epsilon
inputs$clopidogrel$vDAPTScale <- epsilon
#inputs$simvastatin$vScale <- epsilon
inputs$clopidogrel$vRRRepeat.DAPT <- 0 #only for low-weibull runs, to fix retrigger clopidogrel prescription
inputs$vN <- 100
inputs$vHorizon <- 1


####
## 
# Define Simulation Environment.
#
# NOTE: This must be done at a global level for the simmer now() function to be available
#       inside trajectories. Without this at a global level, the simulation won't work.
####
env  <- simmer("RIGHT-v1.1")

exec.simulation <- function(inputs)
{
  set.seed(12345)
  env  <<- simmer("RIGHT-v1.1")
  traj <- simulation(env, inputs)
  env %>% create_counters(counters)
  
  env %>%
    add_generator("patient", traj, at(rep(0, inputs$vN)), mon=2) %>%
    run(365*inputs$vHorizon+1) %>% # Simulate just past horizon
    wrap()
  
  get_mon_arrivals(env, per_resource = T)
}




attributes <- NULL
results <- NULL
for(Istrategy in 0) {
  if(Istrategy==0) 
  {
    inputs$vPreemptive = "None"
    inputs$vReactive = "None"
    inputs$vSwitch = "None"
    inputs$clopidogrel$vDAPT.Start = "Clopidogrel"
  } else if(Istrategy==1) {
    inputs$vPreemptive = "None"
    inputs$vReactive = "None"
    inputs$vSwitch = "None"
    inputs$clopidogrel$vDAPT.Start = "Ticagrelor"
  } else if(Istrategy==2){
    inputs$vPreemptive = "None"
    inputs$vReactive = "None"
    inputs$vSwitch = "All"
    inputs$clopidogrel$vDAPT.Start = "Ticagrelor"
  } else if(Istrategy==3){
    inputs$vPreemptive = "None"
    inputs$vReactive = "Single"
    inputs$vSwitch = "None"
    inputs$clopidogrel$vDAPT.Start = "Clopidogrel"
  } else if(Istrategy==4){    
    inputs$vPreemptive = "None"
    inputs$vReactive = "None"
    inputs$vSwitch = "Genotype"
    inputs$clopidogrel$vDAPT.Start = "Ticagrelor"
  } else if(Istrategy==5){
    inputs$vPreemptive = "None"
    inputs$vReactive = "Single"
    inputs$vSwitch = "None"
    inputs$clopidogrel$vDAPT.Start = "Clopidogrel"
    inputs$clopidogrel$vProbabilityDAPTSwitch <- 1
  } else {    
    inputs$vPreemptive = "None"
    inputs$vReactive = "None"
    inputs$vSwitch = "Genotype"
    inputs$clopidogrel$vDAPT.Start = "Ticagrelor"
    inputs$clopidogrel$vProbabilityDAPTSwitch <- 1
  }
  
  cat("Running ", Istrategy, "\n")
  run <- exec.simulation(inputs)
  run$strategy <- Istrategy
  
  at <- arrange(get_mon_attributes(env),name,key,time)
  at$strategy <- Istrategy
  
  if(is.null(results)) { results <- run } else  {results <- rbind(results, run)}
  if(is.null(attributes)) { attributes <- at } else  {attributes <- rbind(attributes, at)}
}

DT <- data.table(results)
DT[, .N, by = list(resource, strategy)]
summ <- DT[, .N, by = list(resource, strategy)]

source("./costs_ICER.R")
inputs$vN <- 500 #change according to combined count

sum_costs <- NULL
for(i in 0:4) {
  dt <- results %>% filter(strategy==i)
  if(any(dt$resource=="panel_test")==TRUE) {
  dt[dt$resource=="panel_test",]$resource <- "single_test"}
  s <- cost.qaly(dt,inputs) %>% mutate(strategy=i)
  if(is.null(sum_costs)) { sum_costs <- s } else  {sum_costs <- rbind(sum_costs, s)}
}











##########
#Scratch
###########

#Strategy 0
inputs$vPreemptive = "None"
inputs$vReactive = "None"
inputs$vSwitch = "None"
inputs$clopidogrel$vDAPT.Start = "Clopidogrel"

results <- exec.simulation(inputs)
results0 <- results
DT <- data.table(results)
DT[, .N, by = resource]
summary0 <- DT[, .N, by = resource]
arrivals  <- get_mon_arrivals(env, per_resource = T)
arrivals0 <- arrivals
attributes <- arrange(get_mon_attributes(env),name,key,time) 

#Strategy 1
inputs$vPreemptive = "None"
inputs$vReactive = "None"
inputs$vSwitch = "None"
inputs$clopidogrel$vDAPT.Start = "Ticagrelor"

results <- exec.simulation(inputs)
results1 <- results
DT <- data.table(results)
DT[, .N, by = resource]
summary1 <- DT[, .N, by = resource]
arrivals  <- get_mon_arrivals(env, per_resource = T)
arrivals1 <- arrivals
attributes <- arrange(get_mon_attributes(env),name,key,time) 

#Strategy 2
inputs$vPreemptive = "None"
inputs$vReactive = "None"
inputs$vSwitch = "All"
inputs$clopidogrel$vDAPT.Start = "Ticagrelor"

results <- exec.simulation(inputs)
results2 <- results
DT <- data.table(results)
DT[, .N, by = resource]
summary2 <- DT[, .N, by = resource]
arrivals  <- get_mon_arrivals(env, per_resource = T)
arrivals2 <- arrivals
attributes <- arrange(get_mon_attributes(env),name,key,time) 

#Strategy 3
inputs$vPreemptive = "None"
inputs$vReactive = "Single"
inputs$vSwitch = "None"
#inputs$clopidogrel$vProbabilityDAPTSwitch = 1
inputs$clopidogrel$vDAPT.Start <- "Clopidogrel"

results <- exec.simulation(inputs)
results3 <- results
DT <- data.table(results)
DT[, .N, by = resource]
summary3 <- DT[, .N, by = resource]
arrivals  <- get_mon_arrivals(env, per_resource = T)
arrivals3 <- arrivals
attributes <- arrange(get_mon_attributes(env),name,key,time)
#summary3[summary3$resource=="panel_test",]$resource <- "single_test"


#Strategy 4
inputs$vPreemptive = "None"
inputs$vReactive = "None"
inputs$vSwitch = "Genotype"
inputs$clopidogrel$vDAPT.Start = "Ticagrelor"

results <- exec.simulation(inputs)
results4 <- results
DT <- data.table(results)
DT[, .N, by = resource]
summary4 <- DT[, .N, by = resource]
arrivals  <- get_mon_arrivals(env, per_resource = T)
arrivals4 <- arrivals
attributes <- arrange(get_mon_attributes(env),name,key,time) 
