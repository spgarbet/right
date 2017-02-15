library(deSolve)

ss_death <- read.csv("ss-death-2011.csv")

inst_rate <- function(percent, timeframe)
{
  - log(1-percent) / timeframe
}

alt_simp_coef <- function(i)
{
  if (i < 8) stop("Invalid simpson coefficient size")
  
  # pg.117 4.1.14, Numerical Recipes in C, 1st edition
  c(17/48, 50/48, 43/48, 49/48, rep(1, i-8 ), 49/48, 43/48, 50/48, 17/48) 
}


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

Simple <- function(t, y, params)
{
  with(as.list(c(y, params)), {
    
    # Use table for death_prob, Female 40 (offset 1)
    r_d <- if(t >= 80.0) 1 else inst_rate(ss_death$f_death_prob[floor(41+t)] ,1)

    # Event A stops at time t=5 years
    if(t > 5) r_a <- 0
    
    # Event B stops at time 5 years after event A (delay equation)
    #t0 <- if(t < 5) 0 else t-5
    #aPop <- if(t <= 0) 0 else lagvalue(t, 4) - lagvalue(t0, 4) # WRONG FIXE ME!!!!
    
    list(c(
      disc = -disc_rate*disc,            # Simple discount rate
      h = -(r_a+r_d)*h,
      a = r_a*h,
      e1= (1-r_ad)*r_a*h-(r_b+r_d)*e1,
      b = r_b*e1,
      e2= r_b*e1 - r_d*e2,
      d = r_ad*r_a*h + r_d*(h+e1+e2)
    ))
  })
}

yinit <- c(disc=1, h=1, a=0, e1=0, b=0, e2=0, d=0)
times <- seq(0, 90, by=1/365)  # units of years, increments of days
out   <- ode(yinit, times, Simple, params)

# Check sensibility, i.e. all occupancy buckets sum to 1
all((rowSums(out[,c('h','e1','e2','d')]) - 1) < 1e-8)

costs <- function(solution, params)
{
  n <- length(solution[,1])

  with(as.list(params), {
    # Compute Discounted Cost
    cost <- c_a*sum(diff(solution[,'a'])*solution[2:n,'disc']) + # Cost * Number of events in bucket a
            c_b*sum(diff(solution[,'b'])*solution[2:n,'disc']) + # Cost * Number of events in bucket b
            c_t*solution[1, 'h']  # Cost * Initial healthy individuals
    
    # Step size of simulation
    step     <- solution[2,'time'] - solution[1,'time']
    
    # Compute a taper function, for cutting off permanent disutility at time horizon
    taper <- rep(1, n-1)
    to    <- n - 1
    from  <- to - to/5
    taper[(from+1):to] <- seq(1, step, length.out=to/5) # Deals with cutoff in disutility
      
    # Total possible life units is integral of discounted time
    life <- sum(alt_simp_coef(n)*solution[,'disc'])*step
    
    dis  <- d_a*sum(alt_simp_coef(n)*solution[,'a']*solution[,'disc'])*step + # Permanent disutility A (integration)
            d_b*sum(diff(solution[,'b'])*taper*solution[2:n,'disc'])        + # Event B (with taper for horizon)
            sum(alt_simp_coef(n)*solution[,'d']*solution[,'disc'])*step       # Death disutility (integration)
    
    c(cost       = unname(cost),
      qaly       = unname(life - dis),
      possible   = unname(life),
      disutility = unname(dis),
      a_count    = unname(solution[n,'a']),
      b_count    = unname(solution[n,'b']),
      dead_count = unname(solution[n,'d']), 
      living     = unname(solution[n,'h']+solution[n,'e1']+solution[n,'e2'])
      )
  })
}

expected <- function(params) costs(ode(yinit, times, Simple, params), params)

round(expected(params), 4)

params['r_a'] <- params['r_a']*0.5
params['c_t']  <- 2000
round(expected(params), 4)
