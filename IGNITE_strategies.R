setwd("/Users/zilu/Desktop/right-simulation")
source("./run_IGNITE.r")

results <- NULL
for(Istrategy in 0:4) {
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
    inputs$vPreemptive = "Panel"
    inputs$vReactive = "None"
    inputs$vSwitch = "None"
    inputs$clopidogrel$vDAPT.Start = "Clopidogrel"
  } else {    
    inputs$vPreemptive = "None"
    inputs$vReactive = "None"
    inputs$vSwitch = "Genotype"
    inputs$clopidogrel$vDAPT.Start = "Ticagrelor"
    }
  cat("Running ", Istrategy, "\n")
  run <- exec.simulation(inputs)
  run$strategy <- Istrategy
  
  if(is.null(results)) { results <- run } else  {results <- rbind(results, run)}
}

source("./costs_ICER.R")
inputs$vN <- 100 #change according to combined count

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
inputs$vPreemptive = "Panel"
inputs$vReactive = "None"
inputs$vSwitch = "None"
inputs$clopidogrel$vProbabilityDAPTSwitch = 1

results <- exec.simulation(inputs)
results3 <- results
DT <- data.table(results)
DT[, .N, by = resource]
summary3 <- DT[, .N, by = resource]
arrivals  <- get_mon_arrivals(env, per_resource = T)
arrivals3 <- arrivals
attributes <- arrange(get_mon_attributes(env),name,key,time)
summary3[summary3$resource=="panel_test",]$resource <- "single_test"


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
