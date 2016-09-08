results <- read.csv("simvastatin-raw.csv")


source('inputs.R')
source('costs.R')
source('./simvastatin/counters.R')
source("./clopidogrel/counters.R")
source('./warfarin/counters.R')
source("./main/counters.R")
counters <- c(counters.gen, counters.dapt, counters.simvastatin, counters.warfarin)
library(dplyr)
library(plyr)
library(reshape2)

inputs$vN <- 100000
annual_discount_rate <- 0.03

cost.qaly <- function(raw,inputs) 
{
  arrivals <- raw %>%  mutate(name = paste0(name,"_",replication))
  
  # Make all resources a factor (this allows for null events to still get summaries)
  arrivals$resource <- factor(arrivals$resource, counters)
  
  # Adjust all event end times from the inputs$durations
  mapply(function(value, name){
    arrivals[arrivals$resource == name,]$end_time <<-arrivals[arrivals$resource == name,]$start_time + value
  }, value=inputs$durations, name=names(inputs$durations) )
  
  # Truncate to end of study or life
  end_times <- arrivals[arrivals$resource == 'time_in_model',]
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
    arrange(start_time,desc(end_time)) %>% dplyr::mutate(utility = ifelse(row_number()==1,1,NA)) %>% filter(disutility>0 | utility>0) %>% 
    mutate(temp_disutility = as.numeric(disutility>0 & activity_time>0 & activity_time!=1)) 
  
  qaly2 <- qaly1 %>% select(name,start_time,end_time) %>%  melt(id.vars="name") %>% arrange(value) %>% merge(qaly1 %>% select(name,start_time,contains("utility")),by.x=c("name","value"),by.y=c("name","start_time"),all.x=TRUE)
  
  qaly.i <- qaly2  %>% 
    dplyr::mutate(disutility=ifelse(is.na(disutility),0,disutility)) %>%
    dplyr::mutate(utility=pmin(1-disutility)) %>% 
    dplyr::mutate(time=lead(value)) %>%
    dplyr::mutate(time=ifelse(row_number()>1,time-lag(time),time)) %>%  
    filter(!is.na(time)) %>% 
    dplyr::mutate(utility.d = discount_value(utility,A=value,B=max(time,value))) %>% 
    group_by(name) %>%
    dplyr::mutate(qaly.i = pmax(0,utility*time),dQALY=sum(qaly.i)/365.25) %>%
    filter(qaly.i>0)
  
  qaly.unadj <-  arrivals %>% filter(resource=="time_in_model") %>% tbl_df() %>% mutate(QALY=end_time/365.25) %>% select(name,QALY)
  
  qaly.i = qaly.i %>% merge(qaly.unadj,"name",all.x=TRUE)
  
  QALY = qaly.i %>% group_by(name) %>% summarise(dQALY = sum(qaly.i)/365.25) ; QALY
  COST = arrivals %>% group_by(name) %>% summarize(dCOST = sum(discounted_cost))
  
#  qaly.cost = QALY %>% merge(COST,by="name",all.x=TRUE)
  
#  out = list(arrivals=arrivals,qaly= qaly.i,cost = cost.i,summary = qaly.cost)
#  return(out)
 
   return(list(
     dQALY = QALY$dQALY/inputs$vN,
     dCOST = COST$dCOST/inputs$vN
   )) 
}




cost.qaly(subset(results, reactive=='None' & preemptive =='None' ), inputs)
cost.qaly(subset(results, reactive=='Single' & preemptive =='None' ), inputs)
cost.qaly(subset(results, reactive=='None' & preemptive =='Panel' ), inputs)
cost.qaly(subset(results, reactive=='None' & preemptive =='PREDICT' ), inputs)
cost.qaly(subset(results, reactive=='Single' & preemptive =='PREDICT' ), inputs)
cost.qaly(subset(results, reactive=='None' & preemptive =='Age >= 50' ), inputs)
cost.qaly(subset(results, reactive=='Single' & preemptive =='Age >= 50' ), inputs)
