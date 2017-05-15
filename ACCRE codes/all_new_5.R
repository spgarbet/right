### Single Drug - low Weibull
setwd("./right-simulation")
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
rm(list=ls())

#seed_05
sdt <- c(6,
         767898,
         38,
         37,
         510,
         715,
         5333,
         7590,
         74364,
         32070)

args <- commandArgs(trailing = TRUE)
num_seed <- as.integer(args[1])

env  <- simmer("RIGHT-v1.1")

exec.simulation <- function(inputs)
{
  set.seed(sdt[num_seed+1])
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

results <- NULL
#attributes <- NULL
for(preemptive in c("None","PREDICT","Panel"))
{
  for(reactive in c("None","Single","Panel"))
  {
    if(preemptive == "PREDICT" && reactive == "Panel") {next}
    if(preemptive == "PREDICT" && reactive == "Single") {next}
    if(preemptive == "Panel" && reactive == "Single") {next}
    if(preemptive == "Panel" && reactive == "Panel") {next}
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
summ <- DT[, .N, by = list(resource, preemptive, reactive)]
#save(results,file="~gravesj/data/right/all_test_raw.rda")
save(summ,file=paste0("~gravesj/data/right/sum_all_new_",num_seed,"_5.rda"))

###Costs
source("./costs_ICER.R")
inputs$vN <- 200000
s1 <- cost.qaly(subset(results,preemptive=="None" & reactive=="None"),inputs) %>% mutate(strategy="None")
s2 <- cost.qaly(subset(results,preemptive=="None" & reactive=="Single"),inputs) %>% mutate(strategy="Reactive Single")
s3 <- cost.qaly(subset(results,preemptive=="None" & reactive=="Panel"),inputs) %>% mutate(strategy="Reactive Panel")
s4 <- cost.qaly(subset(results,preemptive=="Panel" & reactive=="None"),inputs) %>% mutate(strategy="Universal Preemptive Panel")
s5 <- cost.qaly(subset(results,preemptive=="PREDICT" & reactive=="None"),inputs) %>% mutate(strategy="Targeted Preemptive Panel")

out <- rbind(s1,s2,s3,s4,s5)
save(out,file=paste0("~gravesj/data/right/cost_all_new_",num_seed,"_5.rda"))

###warfarin additional
source("./warfarin/output_cal.R")
print("Warfarin")
warfarin <- warfarin_out(results,inputs)
#warfarin
save(warfarin,file=paste0("~gravesj/data/right/wa_all_new_",num_seed,"_5.rda"))



