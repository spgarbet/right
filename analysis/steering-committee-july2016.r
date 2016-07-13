rm(list=ls())
library(parallel)
setwd("~/Dropbox/Projects/PREDICT/right-simulation")
source("./analysis/extra-functions.r")
vNN = 12500
Scenario.Name = "Baseline"
source("./right-main-simulation-parallel.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vReactive = "None"
inputs$vPreemptive = "None"
source('./main/event_main_loop.R')
s = 12345+proc.time()
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

Baseline_summary  %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))

Scenario.Name = "PREDICT_Panel"
source("./right-main-simulation-parallel.R")
inputs$vNIter = 8
inputs$vN = vNN 
inputs$vReactive = "Panel"
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
                  cbind(strategy= "PREDICT_Panel",PREDICT_Panel_summary %>% summarise(dQALY = mean(dQALY), dCOST=mean(dCOST))))
Results
diff(Results$dCOST) / diff(Results$dQALY)

Sc0 = Baseline_events  %>% count(resource)  
Sc1 = PREDICT_Panel_events  %>% count(resource) 
Delta = Sc0 %>% full_join(Sc1,"resource") %>% mutate(n.y = ifelse(is.na(n.y),0,n.y),n.x=ifelse(is.na(n.x),0,n.x)) %>% mutate(diff = n.y-n.x) %>% data.frame()


dapt= c("clopidogrel","ticagrelor","prasugrel","dapt_switched")
Delta %>% filter(resource %in% dapt)

# # Any Event
# Delta %>% filter(grepl("_event",resource))
# 
# randpat(event="revasc_pci")
# # Bleeding Events
# Delta %>% filter(grepl("bleed_",resource))
# 
# # Stent Thrombosis
# Delta %>% filter(grepl("^st_",resource))
# 
# # MI Events
# Delta %>% filter(grepl("^mi_",resource))
# 
# # Revascularization Events
# Delta %>% filter(grepl("^revasc_",resource))
# 
# 

