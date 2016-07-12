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

exec.sim.parallel <- function(Scenario.Name) 
{
  # Run the Simulation
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
}