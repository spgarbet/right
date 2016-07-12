rm(list=ls())
library(parallel)

####
##
# Setup and Run the Simulation.
##
####
# Get Info on Random Patient Who Experiences a Certain Event


source("./right-main-simulation-parallel.R")
vNN = 20000
inputs$vNIter = 8
inputs$vN = ceiling(vNN/inputs$vNIter)
inputs$vReactive = "None"
inputs$vPreemptive = "None"

# Run the Simulation
source('./main/event_main_loop.R')
s = 12345
    ptm <- proc.time()
sim.run = exec.simulation(s=s)
    (timer2 = proc.time() - ptm)
env= sim.run
arrivals <- get_mon_arrivals(env, per_resource = T) %>%  mutate(name = paste0(name,"_",replication))

# Get Events and Summarize QALYs and Costs
source("./costs.R")
sim.run.stats = cost.qaly(sim.run,inputs)
events = sim.run.stats[["arrivals"]] %>% arrange(start_time,end_time)
costs = sim.run.stats[["cost"]]
qaly = sim.run.stats[["qaly"]]
summ = sim.run.stats[["summary"]]



randpat = function(event="time_in_model",seed=123,pt=NULL)
{
  set.seed(seed)
  ids = events %>% filter(resource==event) %>%  select(name) %>% data.frame()
  id = sample(ids$name,1)
  if (!is.null(pt)) id = pt
  print("Events")
  ee = events %>% filter(name==id)
  cc = costs %>% filter(name==id)
  qq = qaly %>% filter(name==id)
  
  list(events = ee,costs = cc, qaly = qq)
}

randpat(pt="patient12_8")
randpat(event = "st_fatal",seed=12)

