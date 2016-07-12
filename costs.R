

annual_discount_rate <- epsilon
cont_discount_rate   <- -log(1-annual_discount_rate) # Yearly Time Scale
discounted_cost <- function(start_day, end_day, base_yearly_cost, rate = cont_discount_rate)
{
  #base_yearly_cost*(exp(-rate*start_day/365) - exp(-rate*end_day/365))/rate 
 
  base_yearly_cost / (exp(-rate*((end_day-start_day)/365)))
  
}


compile_statistics <- function(env, inputs,replicates= FALSE)
{
  arrivals <- get_mon_arrivals(env, per_resource = T)
  
  if (replicates) arrivals <- arrivals %>%  mutate(name = paste0(name,"_",replication))
  
  # Make all resources a factor (this allows for null events to still get summaries)
  arrivals$resource <- factor(arrivals$resource, counters)
  
  # Adjust all event end times from the inputs$durations
  mapply(function(value, name){
    arrivals[arrivals$resource == name,]$end_time <<-arrivals[arrivals$resource == name,]$start_time + value
  }, value=inputs$durations, name=names(inputs$durations) )
  
  # Truncate to end of study or life
  end_times <- arrivals[arrivals$resource == 'n_patients',]
  arrivals$end_time <- pmin(arrivals$end_time, 
     plyr::join(arrivals[,c("name","end_time")], end_times[,c("name","end_time")], by="name", match="first")[,3])
  # Correct durations for marked events

  # Compute total activity times
  arrivals$activity_time <- arrivals$end_time - arrivals$start_time
  
  # Computes discounted rate of time
  arrivals$discounted_time <- discounted_cost(arrivals$start_time, end_time, 365)
  
  arrivals %>% filter(name==pt)
  
  # Compute Event base cost map
  idx <- function(str) {as.numeric(factor(str, levels=levels(arrivals$resource)))}
  base_cost_map <- rep(0, nlevels(arrivals$resource))
  sapply(names(inputs$costs), FUN=function(name){
    base_cost_map[idx(name)] <<- inputs$costs[[name]]    
  })

  # Compute Disutility cost map
  base_disutility_map <- rep(0, nlevels(arrivals$resource))
  sapply(names(inputs$disutilities), FUN=function(name){
    base_disutility_map[idx(name)] <<- inputs$disutilities[[name]]    
  })
  
  # Multiplying by activity time, not discounted time
  
  arrivals$discounted_cost <- arrivals$activity_time*base_cost_map[as.numeric(arrivals$resource)]
  arrivals$disutility <- arrivals$activity_time*base_disutility_map[as.numeric(arrivals$resource)]
  
  arrivals
}

#####################################################
# Costing Algorithm
#
costs <- function(env, inputs,replicates= FALSE)
{
  
  arrivals <- compile_statistics(env, inputs)
  
  if (replicates) arrivals <- arrivals %>%  mutate(name = paste0(name,"_",replication))
  
  stats <- as.data.frame(do.call(rbind, lapply(split(arrivals, arrivals$name), FUN=function(x)
  {
    life <- x[x$resource=="n_patients",]
    results <- c(sum(x$discounted_cost),
                (sum(life$discounted_time) - sum(x$disutility))/365,
                sum(life$activity_time)/365)
    names(results) <- c("Discount_Cost", "QALY", "Life")
    results
  })))
  
  stats$ce_ratio <- stats$Discount_Cost / stats$QALY
  
  stats
}


#### John's Edits Here
options("scipen"=100, "digits"=4)

annual_discount_rate <- epsilon

discount_value = function(value,ar=annual_discount_rate,A,B)
{
  r <- (1 + ar)^(1/365)-1
  (value/r)*(exp(-r*A)-exp(-r*B))
}
discount = function(value,ar=annual_discount_rate,A) value / (1+ar)^(A/365.25)
####################################################################################################

cost.qaly = function(env,inputs) 
{
  arrivals <- get_mon_arrivals(env, per_resource = T) %>%  mutate(name = paste0(name,"_",replication))
  
  # Make all resources a factor (this allows for null events to still get summaries)
  arrivals$resource <- factor(arrivals$resource, counters)
  
  # Adjust all event end times from the inputs$durations
  mapply(function(value, name){
    arrivals[arrivals$resource == name,]$end_time <<-arrivals[arrivals$resource == name,]$start_time + value
  }, value=inputs$durations, name=names(inputs$durations) )
  
  # Truncate to end of study or life
  end_times <- arrivals[arrivals$resource == 'n_patients',]
  arrivals$end_time <- pmin(arrivals$end_time, 
                            plyr::join(arrivals[,c("name","end_time")], end_times[,c("name","end_time")], by="name", match="first")[,3])
  
  # Compute total activity times
  arrivals$activity_time <- arrivals$end_time - arrivals$start_time
  
  
  # Computes discounted rate of time
  arrivals$discounted_time <- discount_value(value=1,A=arrivals$start_time,B=arrivals$end_time)

  # Compute Event base cost map
  idx <- function(str) {as.numeric(factor(str, levels=levels(arrivals$resource)))}
  base_cost_map <- rep(0, nlevels(arrivals$resource))
  sapply(names(inputs$costs), FUN=function(name){
    base_cost_map[idx(name)] <<- inputs$costs[[name]]    
  })
  
  # Compute Disutility cost map
  base_disutility_map <- rep(0, nlevels(arrivals$resource))
  sapply(names(inputs$disutilities), FUN=function(name){
    base_disutility_map[idx(name)] <<- inputs$disutilities[[name]]    
  })
  names(base_disutility_map) = levels(arrivals$resource)
  
  arrivals$cost <- base_cost_map[as.numeric(arrivals$resource)]
  arrivals$discounted_cost <- ifelse(arrivals$activity_time>0,
                                     discount_value(value=arrivals$cost,A=arrivals$start_time,B=arrivals$end_time),
                                     discount(value = arrivals$cost,A=arrivals$start_time))
                                     
  arrivals$disutility = base_disutility_map[arrivals$resource]
  
  cost.i <- arrivals %>% select(name,resource,start_time,end_time,activity_time,discounted_time,cost,discounted_cost) %>% filter(cost>0)

  qaly1 <- arrivals %>% group_by(name) %>% 
    arrange(start_time,desc(end_time)) %>% mutate(utility = ifelse(row_number()==1,1,NA)) %>% filter(disutility>0 | utility>0) %>% 
    mutate(temp_disutility = as.numeric(disutility>0 & activity_time>0 & activity_time!=1)) 
  
  qaly2 <- qaly1 %>% select(name,start_time,end_time) %>%  melt(id.vars="name") %>% arrange(value) %>% merge(qaly1 %>% select(name,start_time,contains("utility")),by.x=c("name","value"),by.y=c("name","start_time"),all.x=TRUE)
  
  qaly.i <- qaly2  %>%  mutate(disutility=ifelse(is.na(disutility),0,disutility)) %>% mutate(utility=pmin(1-disutility)) %>% 
    mutate(time=lead(value)) %>% mutate(time=ifelse(row_number()>1,time-lag(time),time)) %>%  filter(!is.na(time)) %>% 
    mutate(utility.d = discount_value(utility,A=value,B=max(time,value))) %>% group_by(name) %>% mutate(qaly.i = pmax(0,utility*time),QALY=sum(qaly.i)) 
  
  QALY = qaly.i %>% group_by(name) %>% summarise(dQALY = sum(qaly.i)/365); QALY
  COST = arrivals %>% group_by(name) %>% summarize(dCOST = sum(discounted_cost))
  
  qaly.cost = QALY %>% merge(COST,by="name",all.x=TRUE)
  
  out = list(arrivals=arrivals,qaly= qaly.i,cost = cost.i,summary = qaly.cost)
  return(out)
  
}

