rm(list=ls())
library(parallel)

####
##
# Setup and Run the Simulation.
##
####
source("./right-main-simulation-parallel.R")
inputs$vN = 200
inputs$vNIter = 10
 inputs$vReactive = "None"
 inputs$vPreemptive = "Panel"
source('./main/event_main_loop.R')
# Run the Simulation
s = 12345
ptm <- proc.time()
  sim.run = exec.simulation(s=s)
(timer2 = proc.time() - ptm)
arrivals <- get_mon_arrivals(sim.run, per_resource = T) %>%  mutate(name = paste0(name,"_",replication))
(events <- arrivals %>% count(resource) %>%  data.frame()  ) %>%  filter(grepl("death|_fatal",resource)) 


#### Check Out Individual Patients
(pt = arrivals %>% filter(resource=="revascularized") %>% filter(row_number()==2) %>% select(name) %>% as.character(.))

arrivals %>% filter(name %in% pt) %>% group_by(name)

attributes <- arrange(get_mon_attributes(sim.run),name,key,time) %>%  mutate(name = paste0(name,"_",replication)) 
all.attributes <- spread(attributes %>% group_by(name,key,time) %>% summarize(first = mean(value)),key,first)

all.attributes %>%  filter(name==pt) %>% select(time,contains("DAPT"),-contains("RR"))

source("./costs.R")

stats = compile_statistics(env=sim.run,inputs=inputs,replicates=TRUE); stats %>% filter(name==pt)

costs = costs(env=sim.run,inputs=inputs,replicates=TRUE) ; costs[pt,]


 