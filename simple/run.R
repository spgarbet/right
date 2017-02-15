setwd("./simple")
rm(list=ls())
source("./main_file.R")
source("./costs_simple.R")

## Look at summary statistics
results <- NULL

#can modify here
inputs$vHorizon <- 90
inputs$vN <- 100
inputs$vAge <- 40

for(strategy in c("Standard","Treat")) {
  inputs$vStrategy <- strategy
  cat("Running ", strategy, "\n")
  run <- exec.simulation(inputs)
  run$strategy <- strategy
  if(is.null(results)) { results <- run } else  {results <- rbind(results, run)}
}

DT <- data.table(results)
summary <- DT[, .N, by = list(resource,strategy)]
summary 

s1 <- cost.qaly(subset(results,strategy=="Standard"),inputs) %>% mutate(strategy="Standard")
s2 <- cost.qaly(subset(results,strategy=="Treat"),inputs) %>% mutate(strategy="Treat")
sum_costs <- rbind(s1,s2) %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY)) 
sum_costs