rm(list=ls())
library(parallel)
setwd("~/Dropbox/Projects/PREDICT/right-simulation")
source("./analysis/extra-functions.r")
pkg = list("simmer","dplyr","ggplot2","reshape2","tidyr","msm")
invisible(lapply(pkg, require, character.only = TRUE))

####
##
# Set Number of Patients (per iteration)
##
####
vNN = 125
load("./main/cpi.Rdata")

####
##
# Baseline Scenario
##
####
Scenario.Name = "Baseline"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vReactive = "None"
inputs$vPreemptive = "None"
source("./right-main-simulation-parallel.R")
source('./main/event_main_loop.R')
s = 12345
ptm <- proc.time()
env  = exec.simulation(s=s)
arrivals  <- get_mon_arrivals(env, per_resource = T) %>%  mutate(name = paste0(name,"_",replication))
source("./costs.R")
run.stats = cost.qaly(env,inputs)
events = run.stats[["arrivals"]] %>% arrange(start_time,end_time)
events  %>% filter(name %in% c("patient0_1","patient0_8")) %>% arrange(name)
costs = run.stats[["cost"]]
qaly = run.stats[["qaly"]]
summary = run.stats[["summary"]]
assign(paste0(Scenario.Name,"_sim"),env)
assign(paste0(Scenario.Name,"_events"),events)
assign(paste0(Scenario.Name,"_costs"),costs)
assign(paste0(Scenario.Name,"_qaly"),qaly)
assign(paste0(Scenario.Name,"_summary"),summary)
(timer2 = proc.time() - ptm)


#### 
## 
# PREDICT
##
####
Scenario.Name = "PREDICT_Panel"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vReactive = "Panel"
inputs$vPreemptive = "PREDICT"
source("./right-main-simulation-parallel.R")
source('./main/event_main_loop.R')
s = 12345
ptm <- proc.time()
env  = exec.simulation(s=s)
arrivals  <- get_mon_arrivals(env, per_resource = T) %>%  mutate(name = paste0(name,"_",replication))
source("./costs.R")
run.stats = cost.qaly(env,inputs)
events = run.stats[["arrivals"]] %>% arrange(start_time,end_time)
events  %>% filter(name %in% c("patient0_1","patient0_8")) %>% arrange(name)
costs = run.stats[["cost"]]
qaly = run.stats[["qaly"]]
summary = run.stats[["summary"]]
assign(paste0(Scenario.Name,"_sim"),env)
assign(paste0(Scenario.Name,"_events"),events)
assign(paste0(Scenario.Name,"_costs"),costs)
assign(paste0(Scenario.Name,"_qaly"),qaly)
assign(paste0(Scenario.Name,"_summary"),summary)
(timer2 = proc.time() - ptm)


#### 
## 
# Reactive - Panel
##
####
Scenario.Name = "React_Panel25"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vReactive = "Panel"
inputs$vPreemptive = "None"
inputs$clopidogrel$vProbabilityDAPTSwitch = 1
inputs$clopidogrel$vProbabilityReactive = .25
inputs$simvastatin$vProbabilityReactive = .25

source("./right-main-simulation-parallel.R")
source('./main/event_main_loop.R')
s = 12345
ptm <- proc.time()
env  = exec.simulation(s=s)
arrivals  <- get_mon_arrivals(env, per_resource = T) %>%  mutate(name = paste0(name,"_",replication))
source("./costs.R")
run.stats = cost.qaly(env,inputs)
events = run.stats[["arrivals"]] %>% arrange(start_time,end_time)
events  %>% filter(name %in% c("patient0_1","patient0_8")) %>% arrange(name)
costs = run.stats[["cost"]]
qaly = run.stats[["qaly"]]
summary = run.stats[["summary"]]
assign(paste0(Scenario.Name,"_sim"),env)
assign(paste0(Scenario.Name,"_events"),events)
assign(paste0(Scenario.Name,"_costs"),costs)
assign(paste0(Scenario.Name,"_qaly"),qaly)
assign(paste0(Scenario.Name,"_summary"),summary)
(timer2 = proc.time() - ptm)



#### 
## 
# Reactive - Panel (100%)
##
####
Scenario.Name = "React_Panel75"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vReactive = "Panel"
inputs$vPreemptive = "None"
inputs$clopidogrel$vProbabilityDAPTSwitch = 1
inputs$clopidogrel$vProbabilityReactive = .75
inputs$simvastatin$vProbabilityReactive = .75

source("./right-main-simulation-parallel.R")
source('./main/event_main_loop.R')
s = 12345
ptm <- proc.time()
env  = exec.simulation(s=s)
arrivals  <- get_mon_arrivals(env, per_resource = T) %>%  mutate(name = paste0(name,"_",replication))
source("./costs.R")
run.stats = cost.qaly(env,inputs)
events = run.stats[["arrivals"]] %>% arrange(start_time,end_time)
events  %>% filter(name %in% c("patient0_1","patient0_8")) %>% arrange(name)
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
                  cbind(strategy= "PREDICT_Panel",PREDICT_Panel_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))),
                  cbind(strategy= "React_Panel25",React_Panel25_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))),
                  cbind(strategy= "React_Panel75",React_Panel75_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))))

Results %>% filter(!grepl("Perf",strategy)) %>%  arrange(dCOST)  %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY),dominated = as.integer(ICER<0))

# Compare Event COunts

sc1 = PREDICT_Panel_events
sc2 = React_Panel25_events
compare = merge((Statin_Baseline_eventcount <- sc1 %>% count(resource) %>% data.frame()),(Statin_PGx_eventcount <- sc2 %>% count(resource) %>% data.frame()),"resource",all.x=TRUE,all.y=TRUE) %>% mutate(diff=n.y-n.x)
compare %>% filter(grepl("panel",resource))

