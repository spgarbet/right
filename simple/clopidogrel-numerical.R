library(deSolve)

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


# Estimated Vanderbilt Secular Death first 10 years See:Secular.html
drate <- function(t) inst_rate(0.01272623*exp(0.0940359*t), 1)

# Compute rate exposure for delay differential usage
F_drate <- Vectorize(function(t, years) {
  integrate(drate, lower=max(t-years, 0), upper=min(t, 100))$value
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

pcirate <- function(t)
{
  rep(0.01, length(t))
}

F_pcirate <- function(t)
{
  integrate(pcirate, lower=max(t-1, 0), upper=min(t, 100))$value
}

###################################
# Numerical Delay Differential Equation
Clopidogrel <- function(t, y, params)
{
  with(as.list(c(y, params)), {

    r_d <- drate(t)
    
    # Start of therapy Weibull rate
    incoming <- if (t <= 0)     0 else params$vDAPTShape * t^(params$vDAPTShape - 1) / params$vDAPTScale^params$vDAPTShape
    # Starting 2nd 11 month phase of therapy (basically, minus mortality)
    start2   <- if (t < 30/365) 0 else lagderiv(t-30/365, 2)*exp(-F_drate(t, 30/365))
    # Available at end of 12 months of therapy (phase1+phase2) minus mortality
    avail3   <- if (t < 1)      0 else lagderiv(t-335/365,3)*exp(-F_drate(t, 335/365))
    # Those that finished therapy (minus PCI for last year)
    finish   <- avail3*exp(-F_pcirate(t))
    # Those that restart and continue therapy due to PCi
    start3   <- avail3 - finish
    
    # Those that finished 2nd therapy
    finish2  <- if (t < 1) 0 else lagderiv(t-1, 4)*exp(-F_drate(t, 1))

    list(c(
            notreat = -incoming,
            p1entry = incoming,
            p2entry = start2,
            restart = pcirate(t)*(phase1+phase2),
            phase1  = incoming - r_d*phase1 - start2,
            phase2  = start2   - r_d*phase2 - avail3,
            phase3  = start3   - r_d*phase3 - finish2,
            maint   = finish   - r_d*maint  + finish2,
            mort    = r_d*(phase1+phase2+phase3+maint),
            disc    = -disc_rate*disc            # Simple discount rate
          )
    )
  })
}

yinit <- c(notreat=1, p1entry=0, p2entry=0, restart=0,
           phase1=0,  phase2=0,   phase3=0, maint=0, mort=0, 
           disc=1)
times <- seq(0, 10, by=1/365)  # units of years, increments of days, everyone dies after 120, so simulation is cut short
system.time(out <- dede(yinit, times, Clopidogrel, params))

plot(out)

# Check sensibility, i.e. all occupancy buckets sum to 1
all((rowSums(out[,c('notreat','phase1','phase2', 'phase3', 'maint', 'mort')]) - 1) < 1e-8)
