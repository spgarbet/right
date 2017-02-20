ss_death <- read.csv("ss-death-2011.csv")

inst_rate <- function(percent, timeframe)
{
  - log(1-percent) / timeframe
}

###################################
# Main model parameters
params <- c(
  r_a  = inst_rate(0.1, 5),  # Rate of healthy having event A
  r_b  = inst_rate(0.5, 5),  # Rate of post-A  having event B
  r_ad = 0.05,               # Rate of death as direct result of A
  
  c_a  = 10000,              # Cost of event A
  c_b  = 25000,              # Cost of event B for a year
  c_t  = 0,                  # Cost of therapy
  d_a  = 0.25,               # Permanent disutility for a
  d_b  = 0.1,                # 1-year disutility for b 
  
  disc_rate = 1e-12          # For computing discount
)


death_rate_by_year <- inst_rate(c(ss_death$f_death_prob[41:120],1),1)

pexp_func <- function(rates, intervals, initial=1)
{
  current    <- initial
  boundaries <- c(initial, sapply(1:(length(rates)-1), function(n){
    current <<- current * exp(rates[n] * (intervals[n+1] - intervals[n]))
    current
  }))
  function (t)
  {
    n <- sapply(t, function(x) {
      max(ifelse(x > tail(intervals, 1),
                 length(intervals),
                 min(which(intervals >= x))-1
      ),
      1)
    })
    boundaries[n]*exp(rates[n]*(t - intervals[n]))
  }
}

f <- pexp_func(-death_rate_by_year, 0:80)
curve(f, from=0, to=100)

# Healthy bucket is those who have not died or had event A
# dh    = -(r_a+r_d)*h,


leaving_healthy_rate <- -death_rate_by_year
leaving_healthy_rate[1:5] <- leaving_healthy_rate[1:5] - params['r_a']
h <-  pexp_func(leaving_healthy_rate, 0:80)

curve(h, from=0, to=100)

# Need to run simple-numerical for this to compare.
plot(out[,'time'],out[,'h'], typ='l')
curve(h, add=TRUE, col='red', lty=2)




# Let's compute bucket of A events
# da    = r_a*h
# We only need to do the first 5 years since
# Known Numerical Solution: 0.09963058

# Work out 0 to 1
plot(out[,'time'], out[,'a'], xlim=c(0, 1), typ='l', ylim=c(0, 0.03))
a <- function(t) (params["r_a"] / leaving_healthy_rate[1] )*
                   exp(leaving_healthy_rate[1] * t) -
                   params["r_a"] / leaving_healthy_rate[1]
curve(a, add=TRUE, lty=2, col='red')

# Work out from 1 to 2
a <- function(t) ifelse( t <= 1, 
                         (params["r_a"]*boundaries[1] / leaving_healthy_rate[1] )*
                           exp(leaving_healthy_rate[1] * t) +
                           0.0 - # a(0) boundary condition
                           params["r_a"]*boundaries[1] / leaving_healthy_rate[1],
                         (params["r_a"]*boundaries[2] / leaving_healthy_rate[2] )*
                           exp(leaving_healthy_rate[2] * (t-1)) +
                           0.02083817 -  # a(1) previously
                           params["r_a"]*boundaries[2] / leaving_healthy_rate[2]
                       )
plot(out[,'time'], out[,'a'], xlim=c(0, 2), typ='l', ylim=c(0, 0.05))
curve(a, add=TRUE, lty=2, col='red')

# solution dy/dt = b exp(a t) => y = b/a exp(a t) + c
# https://www.wolframalpha.com/input/?i=dy%2Fdt+%3D+b+exp(a+t)
pexp_int_func <- function(rates1, rates2, intervals, initial1=1, initial2=0)
{
  current    <- initial1

  boundaries <- c(initial1, sapply(1:(length(rates1)-1), function(n){
    current <<- current * exp(rates[n] * (intervals[n+1] - intervals[n]))
    current
  }))

  boundaries <- boundaries * rates2 / rates1
  
  current    <- initial2
  offset     <- sapply(1:length(rates1), function(n) {
    boundary <<- current - boundaries[n]
    current  <<- boundaries[n]* exp(rates1[n]) + boundary
    boundary
  })

  function (t)
  {
    n <- sapply(t, function(x) {
      max(ifelse(x > tail(intervals, 1),
                 length(intervals),
                 min(which(intervals >= x))-1
      ),
      1)
    })
    ifelse(boundaries[n] == 0.0,
           offset[n],
           boundaries[n]*exp(rates1[n]*(t - intervals[n])) + offset[n])
  }
}

event_a_rate <- -leaving_healthy_rate[1:6]

a <- pexp_int_func(event_a_rate, c(rep(params['r_a'], 5), 0), 0:5)
curve(a, from=0, to=5.5)
lines(out[,'time'], out[,'a'], col='red', lty=2)

