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
vNN = 12500
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
inputs$vUpperAge    = 65
inputs$vLowerAge    = 65
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
inputs$vUpperAge    = 65
inputs$vLowerAge    = 65
inputs$vReactive = "None"
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
# Reactive: 100% Compliance
##
####
Scenario.Name = "Reactive_Ideal"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vUpperAge    = 65
inputs$vLowerAge    = 65
inputs$vReactive = "None"
inputs$vPreemptive = "PREDICT"
inputs$clopidogrel$vSensitivityPrDAPT = 1
inputs$clopidogrel$vSpecificityPrDAPT = 1
inputs$clopidogrel$vProbabilityDAPTSwitch = 1
inputs$simvastatin$vPREDICTsens = 1
inputs$simvastatin$vPREDICTspec = 1
inputs$simvastatin$vProbabilityReactive = 1

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
                  cbind(strategy= "Reactive_Ideal",Reactive_Ideal_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))))

icer <- function(results) 
{
  results <- results %>% arrange(dCOST) %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY),dominated = as.integer(ICER<0))
  dominated <- results %>%  filter(dominated==1)
  not.dom <- results %>% filter(dominated==0 | is.na(dominated)) %>%  arrange(dCOST) %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY),ext.dominated = as.integer(ICER<0))
  out = plyr::rbind.fill(not.dom,dominated) %>% arrange(dCOST)
  out
}

icer(Results[c(1,2,3),])

# Compare Event COunts

sc1 = Baseline_events
sc2 = PREDICT_Panel_events
compare = merge((Statin_Baseline_eventcount <- sc1 %>% count(resource) %>% data.frame()),(Statin_PGx_eventcount <- sc2 %>% count(resource) %>% data.frame()),"resource",all.x=TRUE,all.y=TRUE) %>% mutate(diff=n.y-n.x)

dif <- compare 
write.csv(dif,file="~/Desktop/baseline-vs-PREDICT.csv")

# Time to First Drug
foo <- costs  %>% filter(resource=="simvastatin" | resource=="clopidogrel")  %>% arrange(name,start_time)  %>% group_by(name)  %>% filter(row_number()==1) 
quantile(foo$start_time/365,c(0.025,0.1,0.25,0.5,0.75,0.95))
mean(foo$start_time/365)

