rm(list=ls())
library(parallel)
setwd("~/Dropbox/Projects/PREDICT/right-simulation")
source("./analysis/extra-functions.r")

Scenario.Name = "Baseline"
source("./right-main-simulation-parallel.R")
vNN = 10000
inputs$vNIter = 8
inputs$vN = ceiling(vNN/inputs$vNIter)
inputs$vReactive = "None"
inputs$vPreemptive = "None"
source('./main/event_main_loop.R')
s = 12345
ptm <- proc.time()
env  = exec.simulation(s=s)
arrivals  <- get_mon_arrivals(env, per_resource = T) %>%  mutate(name = paste0(name,"_",replication))
source("./costs.R")
run.stats = cost.qaly(env,inputs)
events = run.stats[["arrivals"]] %>% arrange(start_time,end_time)
costs = run.stats[["cost"]]
qaly = run.stats[["qaly"]]
summary = run.stats[["summary"]]
assign(paste0(Scenario.Name,"_sim"),env)
assign(paste0(Scenario.Name,"_events"),events)
assign(paste0(Scenario.Name,"_costs"),costs)
assign(paste0(Scenario.Name,"_qaly"),qaly)
assign(paste0(Scenario.Name,"_summary"),summary)
(timer2 = proc.time() - ptm)

Baseline_summary  %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))

Scenario.Name = "React_Panel"
source("./right-main-simulation-parallel.R")
vNN = 10000
inputs$vNIter = 8
inputs$vN = ceiling(vNN/inputs$vNIter)
inputs$vReactive = "None"
inputs$vPreemptive = "PREDICT"
source('./main/event_main_loop.R')
s = 12345
ptm <- proc.time()
env  = exec.simulation(s=s)
arrivals  <- get_mon_arrivals(env, per_resource = T) %>%  mutate(name = paste0(name,"_",replication))
source("./costs.R")
run.stats = cost.qaly(env,inputs)
events = run.stats[["arrivals"]] %>% arrange(start_time,end_time)
costs = run.stats[["cost"]]
qaly = run.stats[["qaly"]]
summary = run.stats[["summary"]]
assign(paste0(Scenario.Name,"_sim"),env)
assign(paste0(Scenario.Name,"_events"),events)
assign(paste0(Scenario.Name,"_costs"),costs)
assign(paste0(Scenario.Name,"_qaly"),qaly)
assign(paste0(Scenario.Name,"_summary"),summary)
(timer2 = proc.time() - ptm)


Results  =        rbind.data.frame(cbind(strategy  = "Baseline",Baseline_summary  %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))) ,
                  cbind(strategy= "Reactive_Panel",React_Panel_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))))
Results
diff(Results$dCOST) / diff(Results$dQALY)


Baseline_events  %>% count(resource)  %>% filter(grepl("dapt_",resource))
React_Panel_events  %>% count(resource)  %>% filter(grepl("dapt_",resource))


