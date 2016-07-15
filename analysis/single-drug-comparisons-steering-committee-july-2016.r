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
# Clopidogrel Only: Baseline
##
####
Scenario.Name = "Clopidogrel_Baseline"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN
inputs$vUpperAge    = 65
inputs$vHorizon     = 65
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
inputs$vUpperAge    = 65
inputs$vHorizon     = 65
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

Clopidogrel.Compare  =        rbind.data.frame(cbind(strategy  = "Clopidogrel_Baseline",Clopidogrel_Baseline_summary  %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))) ,
                                   cbind(strategy= "Clopidogrel_PGx",Clopidogrel_PGx_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))))

Clopidogrel.Compare %>% filter(!grepl("Perf",strategy)) %>%  arrange(dCOST)  %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY),dominated = as.integer(ICER<0))
merge((Clopidogrel_Baseline_eventcount <- Clopidogrel_Baseline_events %>% count(resource) %>% data.frame()),(Clopidogrel_PGx_eventcount <- Clopidogrel_PGx_events %>% count(resource) %>% data.frame()),"resource") %>% mutate(diff=n.y-n.x)






####
##
# Statin Only: Baseline
##
####
Scenario.Name = "Statin_Baseline"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vUpperAge    = 65
inputs$vHorizon     = 65
inputs$vReactive = "None"
inputs$vPreemptive = "None"
inputs$vDrugs       = list(vSimvastatin = TRUE, 
                           vWarfarin = FALSE,
                           vClopidogrel = FALSE)
inputs$simvastatin$vScale = 1
inputs$simvastatin$vShape = 1

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

Statin_Baseline_events %>% count(resource) %>% data.frame()


####
##
# Statin Only: PGx
##
####
Scenario.Name = "Statin_PGx"
env  <- simmer(Scenario.Name)
source("./inputs.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vUpperAge    = 65
inputs$vHorizon     = 65
inputs$vReactive = "Panel"
inputs$vPreemptive = "None"
inputs$simvastatin$vProbabilityReactive  =1 

inputs$vDrugs       = list(vSimvastatin = TRUE, 
                           vWarfarin = FALSE,
                           vClopidogrel = FALSE)
inputs$simvastatin$vScale = 1
inputs$simvastatin$vShape = 1

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

Statin_PGx_events %>% count(resource) %>% data.frame()


Statin.Compare  =        rbind.data.frame(cbind(strategy  = "Statom_Baseline",Statin_Baseline_summary  %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))) ,
                                               cbind(strategy= "Statin_PGx",Statin_PGx_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))))

Statin.Compare %>% filter(!grepl("Perf",strategy)) %>%  arrange(dCOST)  %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY),dominated = as.integer(ICER<0))





