library(deSolve)
library(flexsurv) # For pgompertz

ss_death <- read.csv("ss-death-2011.csv")

inst_rate <- function(percent, timeframe)
{
  - log(1-percent) / timeframe
}

###################################
# Main model parameters
params = list(
    vPREDICTsens = 0.23,
    vPREDICTspec = 0.93,
    vProbabilityRead       = 1.00, # probability of physician using test results
    vProbabilityReactive   = 1.00, # Under reactive, probability of ordering test
    vProbabilityDAPTSwitch = 0.55, # Source: VUMC PREDICT DATA

    # Population-Level Allele Frequency Distribution
    vCYP2C19.Poor    = 0.21, # (0.15-0.40)
  
    # Indication Paramters (Weibull) source: VUMC data -- files is ./reference/WCS_KM_Distribution_Generation.pdf
    vDAPTShape = 0.59,
    vDAPTScale = 60475.53,
  
    # Stent Thrombosis: Event Rates and Relative Risks
    
    # Relative Risk of ST for patients with loss of function allele who are treated with 
    # Clopidogrel.
    vRR.ST.LOF = 1.75, #(1.50-2.03) High Discrimination Scenario =  2.81 (1.81-4.37)
    
    # The Stent Thrombosis Risks are drawn from a piecewise exponential with the following
    # durations and rates. 
    vRiskST30    = 0.0150, # (0.010-0.020)
    vRiskST365   = 0.0060, # (0.003-0.009)
    vRiskSTgt365 = 0.0022, # (0.001-0.003)
  
    vRR.ST.Alternate = 0.75,
    vRR.ST.Aspirin   = 1.29,

    vST.Case.Fatality = 0.20, #(15-30)
    vPrCABG.ST = 0.10,  # WHAT IS SOURCE?  CAN'T FIND IN ANNALS PAPER...

    # Myocardial Infarction: Event Rates and Relative Risks
    vRiskMI = 0.035, #(0.013-0.097)
    vRR.MI.Alternate =0.84, # (0.75-0.95)
    vPrCABG.MI = 0.08, # (4-12)
    vPrPCI.MI = 0.55, # (45-65)

    # Revascularization
    vRiskRV365   = 0.10, # (0.05-0.15)
    vRiskRVgt365 = 0.03, # (0.02-0.04)
    vPrCABG.RV   = .25, # (15-35)

    # Bleeding FIXME!!!
    vRiskBleed = 0,
    vRiskBleedFatal = 0,
    vRR.Alternative = 0,
    vRR.Fatal.Alternative = 0,
  
    vRiskCABGTIMImajor = 0.022, # (0.013-0.031) 
    vRR.RiskCABGTIMImajor.Alternate = 1.08, # (0.85-1.36)

    vRR.MI.LOF = 1.48, #(1.05-2.07) High Discrimination Scenario = 1.45 (1.09-1.92)
    vRR.Mort.LOF = 1.28, #(0.95-1.73) #Not sure how to use this one
    vRR.Bleed.LOF = 0.84, # (0.75-1.00)
  
    disc_rate = 0.03
)

###################################
# Numerical approach to secular death (very high accuracy!)
f_40yr_percent_d    <- c(ss_death$f_death_prob[41:120])
sim_adj_age         <- 0:79 + 0.5 # 0.5 offset since percentage is for whole year
f_40yr_per_d_spline <- splinefun(sim_adj_age, f_40yr_percent_d)
plot(1:2); dev.off()
curve(f_40yr_per_d_spline, col='red', from=0, to=82, xlab="years past 40", ylab="percent chance of death")
points(sim_adj_age, f_40yr_percent_d)

# Clamped at infinite rate via pmin
f_40yr_drate <- function(t) inst_rate(pmin(f_40yr_per_d_spline(t), 1),1)
curve(f_40yr_drate, from=0, to=90)

####################################
# Integrations of death rates for exposure calculations in delay
# Now, a special function used in delay equation (Had to put upper bound at 81)
F_40yr_drate_5yr <- Vectorize(function(t)
{
  integrate(f_40yr_drate, lower=max(t-5, 0), upper=min(t, 81))$value
})


F_40yr_drate_1yr <- Vectorize(function(t)
{
  integrate(f_40yr_drate, lower=max(t-1, 0), upper=min(t, 81))$value
})


# This is for doing numberical integration of a set of numbers at an even interval
alt_simp_coef <- function(i)
{
  if (i < 8) stop("Invalid Simpson coefficient size")
  
  # pg.117 4.1.14, Numerical Recipes in C, 1st edition
  c(17/48, 50/48, 43/48, 49/48, rep(1, i-8), 49/48, 43/48, 50/48, 17/48) 
}

split <- function(params)
{
  # Probability of going on alternate given the parameters
  with(params, {
    vProbabilityReactive*vProbabilityDAPTSwitch*vCYP2C19.Poor
  })
}

###################################
# Numerical Delay Differential Equation
Clopidogrel <- function(t, y, params)
{
  with(as.list(c(y, params)), {
    # Use table for death_prob, Female 40 (offset 1)
    r_d <- f_40yr_drate(t)
    if(is.infinite(r_d)) r_d <- 1e16 # A really large number as approximation
    
    alt_p    <- split(params) 
    incoming <- if (t <= 0) 0 else params$vDAPTShape * t^(params$vDAPTShape - 1) / params$vDAPTScale^params$vDAPTShape
    off_clop <- if (t < 1) 0 else (1-alt_p)*lagderiv(t-1, 7)*exp(-F_40yr_drate_1yr(t))
    off_alta <- if (t < 1) 0 else (  alt_p)*lagderiv(t-1, 7)*exp(-F_40yr_drate_1yr(t))
    
    mortality_rate <- f_40yr_per_d_spline(t) # This needs to be convolution
    
    list(c(
            disc = -disc_rate*disc,            # Simple discount r ate
            clop = (1-alt_p)*incoming - off_clop - mortality_rate*clop,
            alta = (alt_p)  *incoming - off_alta - mortality_rate*alta,
            mort = mortality_rate*(clop+alta+aspr+good),
            aspr = off_clop + off_alta-mortality_rate*aspr,
            good = -incoming - mortality_rate*good,
            trac = incoming # Useful for tracking incoming.
          )
    )
  })
}

yinit <- c(disc=1, clop=0, alta=0, mort=0, aspr=0, good=1, trak=0)
times <- seq(0, 10, by=1/365)  # units of years, increments of days, everyone dies after 120, so simulation is cut short
system.time(out <- dede(yinit, times, Clopidogrel, params))

plot(out)

# Check sensibility, i.e. all occupancy buckets sum to 1
all((rowSums(out[,c('good','alta','clop', 'aspr', 'mort')]) - 1) < 1e-8)
