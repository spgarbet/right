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

pexp_int_func <- function(rates, intervals, initial=1, factor=1)
{
  current    <- initial
  factor     <- if(length(factor)==1) rep(factor, length(rates)) else factor
  
  boundaries <- c(initial, sapply(1:(length(rates)-1), function(n){
    current <<- current * exp(rates[n] * (intervals[n+1] - intervals[n]))
    current
  }))
  # solution dy/dt = f b exp(a t)
  # https://www.wolframalpha.com/input/?i=dy%2Fdt+%3D+b+exp(a+t)
  boundaries <- boundaries * factor / rates
  
  function (t)
  {
    n <- sapply(t, function(x) {
      max(ifelse(x > tail(intervals, 1),
                 length(intervals),
                 min(which(intervals >= x))-1
      ),
      1)
    })
    boundaries[n]*exp(rates[n]*(t - intervals[n])) - boundaries[n]
  }
}

event_a_rate <- leaving_healthy_rate[1:6]

a <- pexp_int_func(event_a_rate, 0:6, 0, c(rep(params['r_a'], 5), 0))
