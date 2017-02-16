dev.off()

#Setup
#source('run.R')
#source('simple-numerical.R')

par(mfrow=c(2,2))

# Compare Death Curves
data <- subset(results, strategy == "Standard")
n <- length(unique(data$name))
x <- sort(subset(data, resource %in% c("secular_death", "A_death"))$start_time)
plot(x/365, (1:n)/n, typ="l", main="Dead", ylab="Percent Population", xlab="Simulation time (years)")
lines(out[,'time'], out[,'d'], col='red', lty=2)

patients <- subset(data, resource=="B")$name
x <- subset(data, name %in% patients & resource %in% c("B", "secular_death"))
x$value <- 0
x[x$resource == "B",]$value <- 1
x[x$resource == "secular_death",]$value <- -1
x <- x[order(x$start_time),]

plot(x$start_time/365, cumsum(x$value)/n, typ="l", main="Living B sufferers", ylab="Percent Population", xlab="Simulation time (years)")
lines(out[,'time'], out[,'e2'], col='red', lty=2)

x <- sort(subset(data, resource %in% c("A"))$start_time)
plot(x/365, 1:length(x) / n, typ="l", main="A Events", ylab="Percent Population", xlab="Simulation time (years)")
lines(out[,'time'], out[,'a'], col='red',lty=2)

x <- sort(subset(data, resource %in% c("B"))$start_time)
plot(x/365, 1:length(x) / n, typ="l", main="B Events", ylab="Percent Population", xlab="Simulation time (years)")
lines(out[,'time'], out[,'b'], col='red',lty=2)
