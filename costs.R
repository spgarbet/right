

annual_discount_rate <- 0.03
cont_discount_rate   <- -log(1-annual_discount_rate) # Yearly Time Scale
discounted_cost <- function(start_day, end_day, base_yearly_cost, rate = cont_discount_rate)
{
  base_yearly_cost*(exp(-rate*start_day/365) - exp(-rate*end_day/365))/rate 
}

compile_statistics <- function(env, inputs)
{
  arrivals <- get_mon_arrivals(env, per_resource = T)
  
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
  
  # Compute Event base costs
  idx <- function(str) {as.numeric(factor(str, levels=levels(arrivals$resource)))}
  base_cost_map <- rep(0, nlevels(arrivals$resource))
  base_cost_map[idx("drug1")]         <- inputs$vCostDrug1/365
  base_cost_map[idx("drug2")]         <- inputs$vCostDrug2/365
  base_cost_map[idx("drug3")]         <- inputs$vCostDrug3/365
  base_cost_map[idx("drug4")]         <- inputs$vCostDrug4/365
  base_cost_map[idx("genotyped")]     <- inputs$vCostPGx
  base_cost_map[idx("mild_myopathy")] <-   129
  base_cost_map[idx("mod_myopathy")]  <-  2255/30
  base_cost_map[idx("sev_myopathy")]  <- 12811/30
  base_cost_map[idx("cvd")]           <- 20347/30
  
  # Compute Disutility costs
  base_disutility_map <- rep(0, nlevels(arrivals$resource))
  base_disutility_map[idx("mild_myopathy")] <- 0.01
  base_disutility_map[idx("mod_myopathy")]  <- 0.05
  base_disutility_map[idx("sev_myopathy")]  <- 0.53
  base_disutility_map[idx("cvd")]           <- 0.2445
  
  arrivals$discounted_cost <- arrivals$discounted_time*base_cost_map[as.numeric(arrivals$resource)]
  arrivals$disutility <- arrivals$discounted_time*base_disutility_map[as.numeric(arrivals$resource)]
  
  arrivals
}

#####################################################
# Costing Algorithm
#
costs <- function(env, inputs)
{
  arrivals <- compile_statistics(arrivals, inputs)
  
  stats <- as.data.frame(do.call(rbind, lapply(split(arrivals, arrivals$name), FUN=function(x)
  {
    life <- x[x$resource=="n_patients",]
    results <- c(sum(x$discounted_cost),
                (sum(life$discounted_time) - sum(x$disutility))/365,
                sum(life$activity_time)/365)
    names(results) <- c("Discount.Cost", "QALY", "Life")
    results
  })))
  
  stats$ce_ratio <- stats$Discount.Cost / stats$QALY
  
  stats
}
