####### Costs & QALYs
#If run after the main run, no need to reload inputs & counters below.
source('inputs.R')
source('./simvastatin/counters.R')
source("./clopidogrel/counters.R")
source('./warfarin/counters.R')
source("./main/counters.R")
counters <- c(counters.gen, counters.dapt, counters.simvastatin, counters.warfarin)
inputs$vN <- 100000

###########
#library(plyr)
#library(dplyr)
#library(tidyr)
#library(reshape2)
options("scipen"=100, "digits"=10)

#1: genetic tests
#2: drug
#3: AE
#4: other
event_cat <- data.frame(resource=c("panel_test","single_test",
                                  "clopidogrel","ticagrelor","prasugrel","aspirin","warfarin","simvastatin","alt_simvastatin",
                                  "revasc_event","revasc_pci","revasc_cabg","bleed_ext_maj_nonfatal","bleed_int_maj_nonfatal","bleed_min_nonfatal","bleed_fatal",
                                  "st_fatal","st_pci","st_cabg","mi_cabg","mi_pci","mi_med_manage","mild_myopathy","mod_myopathy","sev_myopathy","rahbdo_death",
                                  "cvd","cvd_death","MajorBleed_ICH","MajorBleed_ICH_Fatal","MajorBleed_GI","MajorBleed_GI_Fatal","MajorBleed_Other",
                                  "MajorBleed_Other_Fatal","MinorBleed","Stroke_MinorDeficit","Stroke_MajorDeficit","Stroke_Fatal","DVTPE_Fatal","DVT","PE", "cabg_bleed",
                                  "out_of_range","in_range"),
                       cat=c(rep(1,2),rep(2,7),rep(3,33),rep(4,2))
)

cost.qaly <- function(raw,inputs) 
{
  arrivals <- raw 
  #arrivals <- results %>%  mutate(name = paste0(name,"_",replication))
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
  arrivals$cum_cost <- ifelse(arrivals$activity_time>0,
                                     arrivals$cost*(arrivals$end_time-arrivals$start_time),
                                     arrivals$cost)
  
  arrivals$disutility = base_disutility_map[arrivals$resource]
  
  type <- data.frame(resource=names(inputs$type),type=unlist(inputs$type),row.names=NULL)
  qaly1 <- arrivals %>% group_by(name) %>% 
    arrange(start_time,desc(end_time)) %>% dplyr::mutate(utility = ifelse(row_number()==1,1,NA)) %>% filter(disutility>0 | utility>0) %>% #cross out events that have no impact on utility
    dplyr::select(name,resource,start_time,end_time,activity_time,disutility) %>%
    merge(type,by="resource",all.x=TRUE) %>% #attach type of events: temp vs. permanent disutility
    dplyr::mutate(us=disutility,ue=disutility*(-type)) %>%  #us/ue stand for disutility at start/end time: temp event will add back disutility at end time
    dplyr::select(name,start_time,end_time,us,ue,resource,type) %>% melt(id.vars=c("name","resource","us","ue","type")) %>% arrange(value) %>% #separate and spread start/end time
    dplyr::mutate(disutility=ifelse(variable=="start_time",us,ue)) %>% arrange(name,value,desc(variable)) %>% #match disutility with start/end time
    group_by(name) %>% mutate(time=lead(value)) %>% dplyr::mutate(dtime=ifelse(row_number()>1,time-lag(time),time)) %>% filter(!is.na(dtime)) %>% 
    filter(!(type==0 & dtime==0)) #For events that permanently reduce utility, this deletes double counts of the event and prevent double counting of disutility 
  #For temp event, we need to keep two records (start & end) in the datasets in order to adding back disutility at end time
  
  qaly2 <- qaly1 %>% arrange(name,value,desc(time),variable) %>% mutate(cum1=ifelse(type==1 | is.na(type),0,disutility)) %>% #For permanent events (type==0), pass disutility to accumulate
    group_by(name) %>% mutate(temp_u=1-cumsum(cum1)) %>% 
    dplyr::mutate(cum2=ifelse(type==0 | is.na(type),0,disutility)) %>% mutate(utility=temp_u-cumsum(cum2)) %>% #For temp events, deduct accumulative disutility from temp_u
    filter(utility>0) #do not count negative/zero utility in qaly computation
  
  qaly.i <- qaly2 %>% dplyr::select(name, dtime, utility) %>%
    dplyr::mutate(qaly = utility*dtime) #discounted QALY for each period of time 
  
  #figure out who experience adverse events
  namelist <- raw %>% merge(event_cat,by="resource",all.x = TRUE) %>% filter(cat==3) %>% dplyr::select(name) %>% unique() %>% mutate(event=1)
  
  QALY = qaly.i %>% group_by(name) %>% dplyr::summarise(QALY = sum(qaly)/365.25) %>% 
    merge(namelist,by="name",all.x = TRUE) %>% mutate(event=ifelse(is.na(event),0,1))
  COST.i = arrivals %>% filter(cum_cost>0) %>% group_by(name,resource) %>% dplyr::summarise(cost = sum(cum_cost)) %>% 
    merge(event_cat,by="resource",all.x = TRUE) 
  COST = COST.i %>% group_by(name) %>% dplyr::summarise(COST = sum(cost)) 
  COST.d = COST.i %>% filter(cat==2) %>% group_by(name) %>% dplyr::summarise(COST.d = sum(cost))
  COST <- merge(COST,COST.d,by="name",all.x=T)
  out <- merge(QALY,COST,by="name",all.x=T) %>% 
    mutate(COST=ifelse(!is.na(COST),COST,0),COST.d=ifelse(!is.na(COST.d),COST.d,0)) %>%
    mutate(COST.e=COST-COST.d)
  avgsum <- out %>% group_by(event) %>% dplyr::summarise(N=n(),QALY=mean(QALY),COST = mean(COST),COST.d=mean(COST.d),COST.e=mean(COST.e))
  avgsum2 <- out %>% dplyr::summarise(N=n(),QALY=mean(QALY),COST = mean(COST), COST.d=mean(COST.d),COST.e=mean(COST.e)) %>% mutate(event=9)
  avgsum <- rbind(avgsum,avgsum2) 
  return(avgsum)
}


