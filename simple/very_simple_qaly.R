# Given a results file, compute QALY directly.
arrivals <- results
arrivals$resource <- factor(arrivals$resource, counters)

# A events last past simulation end
arrivals[arrivals$resource == "A",]$end_time <- 365*90

# B events last one year QALY wise
arrivals[arrivals$resource == "B",]$end_time <-arrivals[arrivals$resource == "B",]$start_time + 365

# Truncate to end of life
# NOTE: HAD TO MODIFY DUE TO MULTIPLE STRATEGIES
end_times <- arrivals[arrivals$resource == 'time_in_model',]
arrivals$end_time <- pmin(arrivals$end_time, 
                          plyr::join(arrivals[,c("name","strategy","end_time")], end_times[,c("name","strategy","end_time")], c("name", "strategy"), match="first")[,4])

# Compute total activity times
arrivals$activity_time <- arrivals$end_time - arrivals$start_time

arrivals$QALY <- 0

# Life is worth 1 unit
arrivals[arrivals$resource == "time_in_model",]$QALY <- arrivals[arrivals$resource == "time_in_model","activity_time"]/365.0
arrivals[arrivals$resource == "A",]$QALY <- -0.25*arrivals[arrivals$resource == "A",]$activity_time/365.0
arrivals[arrivals$resource == "B",]$QALY <- -0.1*arrivals[arrivals$resource == "B",]$activity_time/365.0

n <- length(unique(arrivals$name))

sum(arrivals[arrivals$strategy=="Standard","QALY"])/n
sum(arrivals[arrivals$strategy=="Treat","QALY"])/n