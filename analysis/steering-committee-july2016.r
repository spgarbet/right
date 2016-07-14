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
# Clopidogrel Only: Baseline
##
####
Scenario.Name = "Clopidogrel_Baseline"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vReactive = "None"
inputs$vPreemptive = "None"
inputs$vDrugs       = list(vSimvastatin = FALSE, 
                    vWarfarin = FALSE,
                    vClopidogrel = TRUE)
inputs$clopidogrel$vDAPTShape = 1
inputs$clopidogrel$vDAPTScale = 1

source("./right-main-simulation-parallel.R")
source('./main/event_main_loop.R')
s = 1234567
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

Clopidogrel_Baseline_events %>% count(resource) %>% data.frame()



####
##
# Clopidogrel Only: PGx
##
####

Scenario.Name = "Clopidogrel_PGx"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 

inputs$vReactive = "Panel"
inputs$vPreemptive = "None"
inputs$clopidogrel$vProbabilityReactive = 1

inputs$vDrugs       = list(vSimvastatin = FALSE, 
                           vWarfarin = FALSE,
                           vClopidogrel = TRUE)
inputs$clopidogrel$vDAPTShape = 1
inputs$clopidogrel$vDAPTScale = 1

source("./right-main-simulation-parallel.R")
source('./main/event_main_loop.R')
s = 1234567
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

Clopidogrel_PGx_events %>% count(resource) %>% data.frame()

Compare  =        rbind.data.frame(cbind(strategy  = "Clopidogrel_Baseline",Clopidogrel_Baseline_summary  %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))) ,
                                   cbind(strategy= "Clopidogrel_PGx",Clopidogrel_PGx_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))))

Compare %>% filter(!grepl("Perf",strategy)) %>%  arrange(dCOST)  %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY),dominated = as.integer(ICER<0))
merge((Clopidogrel_Baseline_eventcount <- Clopidogrel_Baseline_events %>% count(resource) %>% data.frame()),(Clopidogrel_PGx_eventcount <- Clopidogrel_PGx_events %>% count(resource) %>% data.frame()),"resource") %>% mutate(diff=n.y-n.x)




















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
# PREDICT - Perfect Physician Compliance
##
####
Scenario.Name = "PREDICT_Panel_PerfComp"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vReactive = "Panel"
inputs$vPreemptive = "PREDICT"
inputs$clopidogrel$vProbabilityDAPTSwitch = 1
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
Scenario.Name = "React_Panel"
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


Results  =        rbind.data.frame(cbind(strategy  = "Baseline",Baseline_summary  %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))) ,
                  cbind(strategy= "PREDICT_Panel",PREDICT_Panel_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))),
                  cbind(strategy= "PREDICT_Panel_PerfComp",PREDICT_Panel_PerfComp_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))),
                  cbind(strategy= "React_Panel",PREDICT_Panel_PerfComp_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))))

Results %>% filter(!grepl("Perf",strategy)) %>%  arrange(dCOST)  %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY),dominated = as.integer(ICER<0))

# 
# 
# icer = xx[order(xx$avgcost),]; 
# icer$icer =NA
# icer[2:nrow(icer),]$icer = with(icer,diff(avgcost)/diff(avgqaly)); icer[,c("scenario","avgcost","avgqaly","icer")]
# icer$dominated = icer$ext.dominated =  0
# icer$dominated = as.numeric(icer$icer<0);icer[,c("scenario","avgcost","avgqaly","icer","dominated")]
# icer.d = subset(icer,dominated==1)
# icer = subset(icer,dominated==0 | is.na(dominated)); icer
# icer[2:nrow(icer),]$icer = with(icer,diff(avgcost)/diff(avgqaly)); icer
# icer[which(diff(icer$icer)<0),]$ext.dominated = 1; icer
# icer.d = rbind.data.frame(icer.d,subset(icer,ext.dominated==1)); icer.d
# 
# icer = subset(icer,ext.dominated==0); icer
# icer[2:nrow(icer),]$icer = with(icer,diff(avgcost)/diff(avgqaly)); icer
# ICER = icer[,c("scenario","icer")]; ICER
# 
# 
# 
# 
# 
# Sc0 = Baseline_events  %>% count(resource)  
# Sc1 = PREDICT_Panel_events  %>% count(resource) 
# Delta = Sc0 %>% full_join(Sc1,"resource") %>% mutate(n.y = ifelse(is.na(n.y),0,n.y),n.x=ifelse(is.na(n.x),0,n.x)) %>% mutate(diff = n.y-n.x) %>% data.frame()
# 
# 
# dapt= c("clopidogrel","ticagrelor","prasugrel","dapt_switched")
# Delta %>% filter(resource %in% dapt)
# 
# # # Any Event
# # Delta %>% filter(grepl("_event",resource))
# # 
# # randpat(event="revasc_pci")
# # # Bleeding Events
# # Delta %>% filter(grepl("bleed_",resource))
# # 
# # # Stent Thrombosis
# # Delta %>% filter(grepl("^st_",resource))
# # 
# # # MI Events
# # Delta %>% filter(grepl("^mi_",resource))
# # 
# # # Revascularization Events
# # Delta %>% filter(grepl("^revasc_",resource))
# # 
# # 
# 
# # attributes <- arrange(get_mon_attributes(env),name,key,time) %>% mutate(name=paste0(name,"_",replication))
# # first.attributes <- spread(attributes %>% group_by(name,key) %>% summarize(first = first(value)),key,first)
# # last.attributes <- spread(attributes %>% group_by(name,key) %>% summarize(last = last(value)),key,last)
# # all.attributes <- spread(attributes %>% group_by(name,key,time) %>% summarize(first = mean(value)),key,first)
# # 
# # 
# 
# 
