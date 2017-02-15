library(deSolve)

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
  r_d  = inst_rate(0.1, 5),  # Rate of death 
  r_ad = 0.05,               # Rate of death as direct result of A
  
  c_a  = 10000,              # Cost of event A
  c_b  = 25000,              # Cost of event B for a year
  c_t  = 0,                  # Cost of therapy
  d_a  = 0.25,               # Permanent disutility for a
  d_b  = 0.1,                # 1-year disutility for b 
  
  disc_rate = 0.03           # For computing discount
)

Simple <- function(t, y, params)
{
  with(as.list(c(y, params)), {
    if(t > 5)
    {
      r_a <- 0
      r_d <- 0
    }
    
    list(c(
      disc = -disc_rate*disc,            # Simple discount rate
      h = -(r_a+r_d)*h,
      a = r_a*h,
      e1= (1-r_ad)*r_a*h-(r_b+r_d)*e1,
      b = r_b*e1,
      e2= r_b*e1 - r_d*e2,
      d = (r_d+r_ad*r_a)*h + r_d*e1 + r_d*e2
    ))
  })
}

yinit <- c(disc=1, h=1, a=0, e1=0, b=0, e2=0, d=0)
times <- seq(0, 90, by=1/365)  # units of years, increments of days
out   <- ode(yinit, times, Simple, params)

# Check sensibility, i.e. all occupancy buckets sum to 1
all((rowSums(out[,c('h','e1','e2','d')]) - 1) < 1e-12)

costs <- function(solution, params)
{
  n <- length(solution[,1])

  with(as.list(params), {
    cost <- c_a*solution[n, 'a'] + c_b*solution[n, 'b'] + c_t*solution[1, 'h']
    
    h     <- solution[2,'time'] - solution[1,'time']
    
    # Compute a taper function
    taper <- rep(1, n-1)
    to    <- n - 1
    from  <- to - to/5
    taper[(from+1):to] <- seq(1, h, length.out=to/5) # Deals with cutoff in disutility
      
    dis  <- d_a*sum(alt_simp_coef(n)*solution[,'a'])*h + # Permanent disutility A (integration)
            d_b*sum(diff(solution[,'b'])*taper)        + # Event B (with taper for horizon)
            sum(alt_simp_coef(n)*solution[,'d'])*h       # Death disutility (integration)
    
    c(cost       = cost,
      disutility = dis,
      a_count    = solution[n, 'a'],
      b_count    = solution[n, 'b'],
      dead_count = solution[n, 'd'], 
      living     = solution[n, 'h']+solution[n,'e1']+solution['e2']
      )
  })
}

expected <- function(params) costs(ode(yinit, times, Simple, params), params)

expected(params)

params['r_a'] <- params['r_a']*0.5
params['c_t']  <- 2000
expected(params)
