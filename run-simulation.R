rm(list=ls())
library(parallel)

####
##
# Setup and Run the Simulation.
##
####
source("./right-main-simulation-parallel.R")
inputs$vN = 200
inputs$vNIter = 1
 inputs$vReactive = "None"
 inputs$vPreemptive = "None"
source('./main/event_main_loop.R')
# Run the Simulation
s = 12345
ptm <- proc.time()
  sim.run = exec.simulation(s=s)
(timer2 = proc.time() - ptm)

source("./costs.R")
sim.run.stats = cost.qaly(sim.run,inputs)
events = sim.run.stats[["arrivals"]]
costs = sim.run.stats[["cost"]]
qaly = sim.run.stats[["qaly"]]
summ = sim.run.stats[["summary"]]

summ %>% summarise(qaly = mean(dQALY),cost=mean(dCOST))

pt = events %>% filter(resource=="cabg_mi") %>% select(name) %>% filter(row_number()==1) %>% as.character()
events %>% filter( name==pt )
costs %>% filter(name==pt) 
qaly %>% filter(name==pt)
