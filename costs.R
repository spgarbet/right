

annual_discount_rate <- epsilon
cont_discount_rate   <- -log(1-annual_discount_rate) # Yearly Time Scale
discounted_cost <- function(start_day, end_day, base_yearly_cost, rate = cont_discount_rate)
{
  base_yearly_cost*(exp(-rate*start_day/365) - exp(-rate*end_day/365))/rate 
  #(base_yearly_cost/365) / (1 + (rate/365))^(365*(start_day-end_day))
}

discounted_cost(start_day = 0,end_day=1,base_yearly_cost=250*365,rate=epsilon)

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
  arrivals$discounted_time <- discounted_cost(arrivals$start_time, arrivals$end_time, 365.0)
  
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
  
  arrivals$discounted_cost <- arrivals$discounted_time*base_cost_map[as.numeric(arrivals$resource)]
  arrivals$disutility <- arrivals$discounted_time*base_disutility_map[as.numeric(arrivals$resource)]
  
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
