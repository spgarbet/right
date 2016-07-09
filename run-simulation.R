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
inputs$vDrugs$vSimvastatin = FALSE
# inputs$vReactive = "None"
# inputs$vPreemptive = "Single"
source('./main/event_main_loop.R')
# Run the Simulation
ptm <- proc.time()
  sim.run = exec.simulation(s=123456)
(timer2 = proc.time() - ptm)
arrivals <- get_mon_arrivals(sim.run, per_resource = T)
(events <- arrivals %>% count(resource) %>%  data.frame()  ) %>%  filter(grepl("death|_fatal",resource)) 
events %>% filter(grepl("_test",resource))



