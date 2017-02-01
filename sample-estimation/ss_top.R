#setwd("./right-simulation")
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
#load(file="~gravesj/data/right/results_ss_0.rda")
#results0 <- results %>%  mutate(name = paste0(name,"_b0"))
#rm(results)
#load(file="~gravesj/data/right/results_ss_1.rda")
#results1 <- results %>%  mutate(name = paste0(name,"_b1"))
#rm(results)
#load(file="~gravesj/data/right/results_ss_2.rda")
#results2 <- results %>%  mutate(name = paste0(name,"_b2"))
#rm(results)
#load(file="~gravesj/data/right/results_ss_3.rda")
#results3 <- results %>%  mutate(name = paste0(name,"_b3"))
#rm(results)
#load(file="~gravesj/data/right/results_ss_4.rda")
#results4 <- results %>%  mutate(name = paste0(name,"_b4"))
#rm(results)
#load(file="~gravesj/data/right/results_ss_5.rda")
#results5 <- results %>%  mutate(name = paste0(name,"_b5"))
#rm(results)
#load(file="~gravesj/data/right/results_ss_6.rda")
#results6 <- results %>%  mutate(name = paste0(name,"_b6"))
#rm(results)
#load(file="~gravesj/data/right/results_ss_7.rda")
#results7 <- results %>%  mutate(name = paste0(name,"_b7"))
#rm(results)
#load(file="~gravesj/data/right/results_ss_8.rda")
#results8 <- results %>%  mutate(name = paste0(name,"_b8"))
#rm(results)
#load(file="~gravesj/data/right/results_ss_9.rda")
#results9 <- results %>%  mutate(name = paste0(name,"_b9"))
#rm(results)
#results <- rbind(results0,results1,results2,results3,results4,results5,results6,results7,results8,results9)
#save(results,file="~gravesj/data/right/results_raw_ss_10.rda")
#load(file="~gravesj/data/right/results_raw_ss_10.rda")

###events summary
DT <- data.table(results)
print("Summary")
summary <- DT[, .N, by = list(resource, preemptive, reactive)]
#save(summary,file="~gravesj/data/right/results_sum_ss_10.rda")

###Costs
source('inputs.R')
source('./simvastatin/counters.R')
source("./clopidogrel/counters.R")
source('./warfarin/counters.R')
source("./main/counters.R")
counters <- c(counters.gen, counters.dapt, counters.simvastatin, counters.warfarin)

source("./costs_new.R")
inputs$costs$panel_test <- 0 #to get ideal genetic information scenario

s1 <- cost.qaly(subset(results,preemptive=="None" & reactive=="None"),inputs) %>% mutate(strategy="None")
s2 <- cost.qaly(subset(results,preemptive=="Panel" & reactive=="None"),inputs) %>% mutate(strategy="Genotyped")
print("sum_costs")
rbind(s1[s1$event!=9,],s2[s1$event!=9,]) %>% 
  recast(strategy ~ variable + event, id.var = c("strategy", "event")) %>%
  mutate(rAE=N_1/(N_1+N_0),diffQALY=QALY_1-QALY_0,diffCOST=COST_1-COST_0) %>%
  mutate(deltar=rAE-lag(rAE),deltaQ=diffQALY-lag(diffQALY),deltaC=diffCOST-lag(diffCOST))
rbind(s1[s1$event==9,],s2[s2$event==9,]) %>% mutate(NMB=(QALY*50000)-COST) %>% mutate(diff=NMB-lag(NMB))



