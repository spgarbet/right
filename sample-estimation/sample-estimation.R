setwd("graves")


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


d <- read.csv("simvastatin-raw.csv") %>% mutate(name = paste0(name,"_",replication))
d$resource <- factor(d$resource, counters)

# Adjust all event end times from the inputs$durations
mapply(function(value, name){
  d[d$resource == name,]$end_time <<-d[d$resource == name,]$start_time + value
}, value=inputs$durations, name=names(inputs$durations) )

# Truncate to end of study or life
end_times <- d[d$resource == 'time_in_model',]
d$end_time <- pmin(d$end_time, 
  plyr::join(d[,c("name","end_time")], end_times[,c("name","end_time")], by="name", match="first")[,3])

# Compute total activity times
d$activity_time <- d$end_time - d$start_time


# Computes discounted rate of time
d$discounted_time <- discount_value(value=1,A=d$start_time,B=d$end_time)

# Compute Event base cost map
idx <- function(str) {as.numeric(factor(str, levels=levels(d$resource)))}
base_cost_map <- rep(0, nlevels(d$resource))
sapply(names(inputs$costs), FUN=function(name){
  base_cost_map[idx(name)] <<- inputs$costs[[name]]    
})

# Compute Disutility cost map
base_disutility_map <- rep(0, nlevels(d$resource))
sapply(names(inputs$disutilities), FUN=function(name){
  base_disutility_map[idx(name)] <<- inputs$disutilities[[name]]    
})
names(base_disutility_map) = levels(d$resource)

d$cost <- base_cost_map[as.numeric(d$resource)]
d$discounted_cost <- ifelse(d$activity_time>0,
  discount_value(value=d$cost,A=d$start_time,B=d$end_time),
  discount(value = d$cost,A=d$start_time))

d$disutility = base_disutility_map[d$resource]  



cost.qaly <- function(arrivals,N) 
{

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

  c(dQALY=QALY$dQALY,dCOST=COST$dCOST) / N
}


library(boot)

uname <- unique(d$name)

b.estimate <- function(N)
{
  dd <- subset(d, name %in% sample(uname, N) & preemptive == "None" & reactive == "None" )
  statistic <- function(x, which)
  {
    patients <- x[which]
    n        <- length(which)
    cost.qaly(subset(dd, name %in% patients ), n)
  }
  boot(unique(dd$name), statistic, R=1000)
}

x <- floor(10^(seq(1, 5, by=0.5)))
results <- lapply(x, FUN=b.estimate)
