library(deSolve)
library(flexsurv) # For pgompertz

ss_death <- read.csv("ss-death-2011.csv")


inst_rate <- function(percent, timeframe)
{
  - log(1-percent) / timeframe
}


###################################
# Numerical approach to secular death (very high accuracy!)
f_40yr_percent_d    <- c(ss_death$f_death_prob[41:120])
sim_adj_age         <- 0:79
f_40yr_per_d_spline <- splinefun(sim_adj_age, f_40yr_percent_d)
dev.off()
curve(f_40yr_per_d_spline, col='red', from=0, to=82, xlab="years past 40", ylab="percent chance of death")
points(sim_adj_age, f_40yr_percent_d)

# Clamped at infinite rate via pmin
f_40yr_drate <- function(t) inst_rate(pmin(f_40yr_per_d_spline(t), 1),1)
curve(f_40yr_drate, from=0, to=90)

###################################
# Use exact same Gompertz approx as DES instead, but work above is very nice
f_40yr_drate <- function(t) dgompertz(t, 0.100751, 0.000837072) / (1-pgompertz(t, 0.100751, 0.000837072))
curve(inst_rate(pmin(f_40yr_per_d_spline(x), 1),1), col='red', lty=2, from=0, to=90)
curve(f_40yr_drate, add=TRUE)

####################################
# Integrations of death rates for exposure calculations in delay
# Now, a special function used in delay equation (Had to put upper bound at 81)
F_40yr_drate <- Vectorize(function(t) 
{
  integrate(f_40yr_drate, lower=max(t-5, 0), upper=min(t, 81))$value
})

# A second special equation for death due to 
F_40yr_rate_int <- Vectorize(function(lower, upper) 
{
  integrate(f_40yr_drate, lower=max(lower, 0), upper=min(upper, 81))$value
})

# This is for doing numberical integration of a set of numbers at an even interval
alt_simp_coef <- function(i)
{
  if (i < 8) stop("Invalid Simpson coefficient size")
  
  # pg.117 4.1.14, Numerical Recipes in C, 1st edition
  c(17/48, 50/48, 43/48, 49/48, rep(1, i-8 ), 49/48, 43/48, 50/48, 17/48) 
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

###################################
# Numerical Delay Differential Equation
Simple <- function(t, y, params)
{
  with(as.list(c(y, params)), {
    
    # Use table for death_prob, Female 40 (offset 1)
    r_d <- f_40yr_drate(t)
    if(is.infinite(r_d)) r_d <- 1e16 # A really large number

    # Event B stops at time 5 years after event A (delay equation)
    dd_b <- if (t < 5 || t> 10) 0 else (1-r_ad)*r_a*lagvalue(t-5, 2)*exp(-5*r_b - F_40yr_drate(t)) 
    
    # Event A stops at time t=5 years
    if(t > 5) r_a <- 0
      
    list(c(
            disc = -disc_rate*disc,            # Simple discount r ate
            h    = -(r_a+r_d)*h,
            a    = r_a*h,
            e10  = (1-r_ad)*r_a*h-r_d*e10-dd_b-r_b*e10,
            e15  = dd_b - r_d*e15,
            b    = r_b*e10,
            e2   = r_b*e10 - r_d*e2,
            d    = r_ad*r_a*h + r_d*(h+e10+e15+e2),
            db   = r_b*e10 - r_d*db - if (t < 1) 0 else lagderiv(t-1, 6)*exp(-F_40yr_rate_int(t-1, t))
          )
    )
  })
}

yinit <- c(disc=1, h=1, a=0, e10=0, e15=0, b=0, e2=0, d=0, db=0)
times <- seq(0, 80, by=1/365)  # units of years, increments of days, everyone dies after 120, so simulation is cut short
out   <- dede(yinit, times, Simple, params)

plot(out)

# Check sensibility, i.e. all occupancy buckets sum to 1
all((rowSums(out[,c('h','e10','e15', 'e2','d')]) - 1) < 1e-8)

costs <- function(solution, params)
{
  n <- length(solution[,1])
  simpson <- alt_simp_coef(n)

  with(as.list(params), {
    # Compute Discounted Cost
    cost <- c_a*sum(diff(solution[,'a'])*solution[2:n,'disc']) + # Cost * Number of events in bucket a
            c_b*sum(diff(solution[,'b'])*solution[2:n,'disc']) + # Cost * Number of events in bucket b
            c_t*solution[1, 'h']  # Therapy Cost * Initial healthy individuals
    
    # Step size of simulation
    step     <- solution[2,'time'] - solution[1,'time']
    
    # Total possible life units is integral of discounted time
    life <- sum(simpson*solution[,'disc'])*step
    
    # Permanent disutility for A (integration)
    disA <- d_a*sum(simpson*solution[,'e10']*solution[,'disc'])*step + 
            d_a*sum(simpson*solution[,'e15']*solution[,'disc'])*step +
            d_a*sum(simpson*solution[,'e2' ]*solution[,'disc'])*step
    
    # Event B
    disB <- d_b*sum(simpson*solution[,'db']*solution[,'disc'])*step

    # Death disutility
    disD <- sum(simpson*solution[,'d']*solution[,'disc'])*step       # Death disutility (integration)
    
    c(cost       = unname(cost),
      qaly       = unname(life-disA-disB-disD),
      possible   = unname(life),
      disutility = unname(disA+disB+disD),
      a_count    = unname(solution[n,'a']),
      disutil_a  = unname(disA),
      b_count    = unname(solution[n,'b']),
      disutil_b  = unname(disB),
      dead_count = unname(solution[n,'d']), 
      disutil_d  = unname(disD),
      living     = unname(solution[n,'h']+solution[n,'e10']+solution[n,'e15']+solution[n,'e2'])
      )
  })
}

expected <- function(params) costs(dede(yinit, times, Simple, params), params)

round(expected(params), 4)

params['r_a'] <- params['r_a']*0.5
params['c_t']  <- 2000
round(expected(params), 4)

# Some diagnostic plots
# 
# dev.off()
# plot(out[,'time'],out[,'db'], typ='l', xlim=c(0, 12))
# abline(h=0, col='red', lty=2)
# abline(v=11, col='blue', lty=2)
# abline(v=5, col='green', lty=2)
# 
# dev.off()
# plot(out[,'time'],out[,'b'], typ='l', xlim=c(0, 12))
# abline(v=10, col='green', lty=2)
# 
# dev.off()
# plot(out[,'time'],out[,'e10'], typ='l', xlim=c(0, 12))
# abline(v=10, col='green', lty=2)