setwd("./right-simulation")
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

cost <- NULL
for(j in 1:5) {
for(i in 0:9) {
  load(file=paste0("~gravesj/data/right/cost_all_new_",i,"_",j,".rda"))
  out <- out %>%  mutate(name = paste0(name,"_b",i,"_",j))
  if(is.null(cost)) { cost <- out } else  {cost <- rbind(cost, out)}
}}

#replace zero
cost <- cost %>% mutate(total=ifelse(is.na(total),0,total)) %>%
  mutate(test=ifelse(is.na(test),0,test)) %>%
  mutate(drug=ifelse(is.na(drug),0,drug)) %>%
  mutate(event=ifelse(is.na(event),0,event)) 

sum_costs <- cost %>% group_by(strategy) %>%
  summarise(QALY=mean(dQALY),total=mean(total),test=mean(test),drug=mean(drug),event=mean(event)) %>%
  arrange(strategy) %>%
  mutate(ICER = (total-total[1])/(QALY-QALY[1]))
sum_costs
save(cost,file="~gravesj/data/right/raw_cost_all50_new_1.rda")
save(sum_costs,file="~gravesj/data/right/cost_all50_new_1.rda")

###ICER
icer <- function(results) 
{
  x <- results %>% arrange(total) %>% mutate(ICER = (lag(total)-total)/(lag(QALY)-QALY)) 
  
  #strong dominance (cross out strategies with a negative ICER)
  str.dom <- NULL
  if(any(x$ICER[-1]<0)==FALSE) {
    x$dominated[2:(nrow(x))] = 0
  } 
  while(any(x$ICER[-1]<0))
  {
    y <- x %>% filter(ICER<0)
    x <- x %>% filter(ICER>0 | is.na(ICER)) %>% arrange(total) %>% mutate(ICER = (total-lag(total))/(QALY-lag(QALY)))
    x$dominated[2:(nrow(x))] = 0
    str.dom <- rbind.fill(str.dom, y)
  }
  if(is.null(str.dom)==FALSE) {str.dom <- str.dom %>% mutate(ICER=NA, dominated=1)}
  
  #extended dominance (cross out weakly dominated strategies until ICERs always increase with costs)
  ext.dom <- NULL
  while(any(order(x$ICER[-1])!=1:(nrow(x)-1))) 
  {
    r <- nrow(x)
    x$ext.dominated <- NA
    for (i in 2:(r-1)) {
      x$ext.dominated[i] = as.integer(x$ICER[i] > x$ICER[i+1])   
    }
    y <- x %>% filter(ext.dominated==1)
    x <- x %>% filter(ext.dominated==0 | is.na(ext.dominated)) %>% arrange(total) %>% mutate(ICER = (total-lag(total))/(QALY-lag(QALY)))
    ext.dom <- rbind.fill(ext.dom, y)
  }
  if(is.null(ext.dom)==FALSE) {ext.dom <- ext.dom %>% mutate(ICER=NA, ext.dominated=1) }
  
  out = plyr::rbind.fill(x, str.dom, ext.dom) %>% arrange(total)
  out
}

re2 <- icer(sum_costs)
re2
save(re2,file="~gravesj/data/right/icer_all50_new_1.rda")

###event counts
event <- NULL
for(j in 1:5) {
  for(i in 0:9) {
  load(file=paste0("~gravesj/data/right/sum_all_new_",i,"_",j,".rda"))
  if(is.null(event)) { event <- summ} else  {event <- rbind(event, summ)}
}}
event <- event %>% group_by(resource,preemptive,reactive) %>% summarise(N=sum(N))
save(event,file="~gravesj/data/right/summ_all50_new_1.rda")

###warfarin additional
source("./warfarin/output_cal.R")
war <- NULL
for(j in 1:5) {
  for(i in 0:9) {
  load(file=paste0("~gravesj/data/right/wa_all_new_",i,"_",j,".rda"))
  warfarin <- warfarin %>% mutate(name = paste0(name,"_b",i,"_",j))
  if(is.null(war)) {war <- warfarin} else  {war <- rbind(war, warfarin)}
}}
warfarin <- war %>% group_by(preemptive,reactive) %>%
  summarise(pctp=mean(pctp),test=mean(test)) 
warfarin
save(warfarin,file="~gravesj/data/right/wa_all50_new_1.rda")
