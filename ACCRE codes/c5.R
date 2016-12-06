### 3-Drug Run
setwd("./right-simulation")
pkg = list("simmer","ggplot2","reshape2","tidyr","data.table","downloader","msm")
invisible(lapply(pkg, require, character.only = TRUE))

rm(list=ls())

env  <- simmer("RIGHT-v1.1")

exec.simulation <- function(inputs)
{
  set.seed(3905)
  env  <<- simmer("RIGHT-v1.1")
  traj <- simulation(env, inputs)
  env %>% create_counters(counters)
  
  env %>%
    add_generator("patient", traj, at(rep(0, inputs$vN)), mon=2) %>%
    run(365*inputs$vHorizon+1) %>% # Simulate just past horizon
    wrap()
  
  get_mon_arrivals(env, per_resource = T)
}

source("./extra.R")
options(digits=5)
inputs$vN <- 200000

###Single Drug 
inputs$vDrugs = list(vSimvastatin = T, 
                     vWarfarin = T,
                     vClopidogrel = T)
#inputs$warfarin$vscale_timetowarfarin <- epsilon
inputs$clopidogrel$vDAPTScale <- epsilon
#inputs$simvastatin$vScale <- epsilon
inputs$clopidogrel$vRRRepeat.DAPT <- 0 #only for low-weibull runs, to fix retrigger clopidogrel prescription

results <- NULL
#attributes <- NULL
for(preemptive in "None")
{
  for(reactive in c("None","Panel"))
  {
    #if(preemptive == "PREDICT" && reactive == "Panel") {next}
    #if(preemptive == "PREDICT" && reactive == "Single") {next}
    #if(preemptive == "Panel" && reactive == "Single") {next}
    #if(preemptive == "Panel" && reactive == "Panel") {next}
    #cat("Running ", preemptive, "/", reactive, "\n")
    
    inputs$vPreemptive <- preemptive
    inputs$vReactive   <- reactive
    run <- exec.simulation(inputs)
    run$preemptive <- preemptive
    run$reactive   <- reactive
    
    #at <- arrange(get_mon_attributes(env),name,key,time)
    #at$preemptive <- preemptive
    #at$reactive   <- reactive
    
    if(is.null(results)) { results <- run } else  {results <- rbind(results, run)}
    #if(is.null(attributes)) { attributes <- at } else  {attributes <- rbind(attributes, at)}
  }
}

###events summary
DT <- data.table(results)
print("Summary")
DT[, .N, by = list(resource, preemptive, reactive)]
save(results,file="~gravesj/data/right/results_c5.rda")

