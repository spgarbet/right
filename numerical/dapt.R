params <- list(
# Costs / QALY discounting rate
  discount = 0.03,
  
# Weibull entry rate to treatment from healthy population
  shape = 0.59,
  scale = 60475.53,
  
# phi is fatality rate
# p is primary treatment, m is maintenance treatment compartment
# 1 is first 30 days, 2 is first year, 3 is 1+ year compartment
  phi_p = rnorm(3, 0.1, 0.025),
  phi_m = rnorm(3, 0.1, 0.025),

# kappa for CABG rates, aka fall out of primary to maintenence
  kappa_p = rnorm(3, 0.1, 0.025),
  kappa_m = rnorm(3, 0.1, 0.025),
  
# PCI rate, i.e. restarting primary treatment rate
  pi_p = rnorm(3, 0.1, 0.025), 
  pi_m = rnorm(3, 0.1, 0.025),
  
# Chi Daily costs for each compartment
  chi_p = rnorm(3, 0.1, 0.025),
  chi_m = rnorm(3, 0.1, 0.025),

# rho daily QALY penalty
  rho_p = rnorm(3, 0.1, 0.025),
  rho_m = rnorm(3, 0.1, 0.025),

# psi permanent QALY penalty
  psi_p = rnorm(3, 0.1, 0.025),
  psi_m = rnorm(3, 0.1, 0.025)
)

# Primary loss from f due to PCI, CABG and fatality
params$f_loss    <- with(params, {phi_p + kappa_p + pi_p})
params$g_loss    <- params$f_loss
params$g_loss[3] <- params$phi_m[3]

###################################
# Numerical Age Structured Solution.
fence <- function(x, lb, ub) pmax(lb, pmin(x, ub))

# Weibull functions with boundaries built in
# Only valid for t > 0
w <- function(t, params) dweibull(t, params$shape, params$scale/365)
W <- function(t, params) pweibull(t, params$shape, params$scale/365)

# Death as ratio of whole at time t in years
# Estimated by convolution of arrival age distribution with SS secular death rates
# Only valid for 10 years in present formulation
s <- function(t) exp(0.01272623*(1-exp(0.0940359*t))/0.0940359)

# Survival of healthy individual population, t in years
lambda <- function(t, params) s(t) * (1 - W(t, params))

# Loss in each domain via exponential process
loss <- Vectorize(vectorize.args="a", FUN=function(a, rates)
{
  prod(exp(c(-fence(a,        0,  30/365),
             -fence(a-30/365, 0, 335/365),
             -fence(a-1,      0, Inf    ))*rates))
})

# Derivative of loss (ignores discontinuities)
dloss <- function(a, rates)
{
  # Risk phase of therapy age
  i <- ifelse(a <= 30/365, 1, ifelse(a <= 1, 2, 3))

  -rates[i]*loss(a, rates) 
}

# Exposure
exposed <- function(a, rates)
{
  left  <- max(a-1, 0)
  mon   <- 30/365
  sum(
    c(if(left > 30/365) 0 else min(mon, a) - left,
      if(left <= 1 && a >= mon) min(1, a) - max(30/365, left) else 0,
      if(a < 1) 0 else a - max(1, left)
    )*rates
  )
}


# Primary compartment
f_a <- function(a, params) if(a < 0 || a > 0) 0 else 

# Solve characteristic starting with 1 patient at t=0 only for boundary

library(deSolve)


clopidogrel <- function(a, y, params)
{
  with(as.list(c(y,params)), {
    
    # Risk phase of therapy age
    i <- if(a <= 30/365) 1 else
         if(a <= 1)      2 else
                         3
    
    # Forcing function for primary compartment (f -> g)
    f   <- loss(a, f_loss)
    f_a <- if(a > 1) 0 else f
    g_a <- if(a > 1) loss(a, g_loss) else 0
    
    # Compute PCI completion
    exposure <- exp(-exposed(a, f_loss))
    cp_1 <- if(a < 1) 0 else lagderiv(a-1,  8)*exposure
    cp_2 <- if(a < 1) 0 else lagderiv(a-1,  9)*exposure
    cp_3 <- if(a < 1) 0 else lagderiv(a-1, 10)*exposure
    cp_4 <- if(a < 1) 0 else lagderiv(a-1, 11)*exposure
    cp_5 <- if(a < 1) 0 else lagderiv(a-1, 12)*exposure
    
    cabg_r <- kappa_p[i]*(f_a + pci_1 + pci_2 + pci_3 + pci_4 + pci_5)+pi_p[i]*pci_5

    list(c(
      m     = cabg_r + cp_1 + cp_2 + cp_3 + cp_4 + cp_5 - phi_m[i]*m, # maintenance from CABG
      cabg  = cabg_r + kappa_m[i]*(m+g_a),               # total CABG events
      pci_1 = pi_p[i]*(f_a   - pci_1) - phi_p[i]*pci_1 - kappa_p[i]*pci_1 - cp_1,
      pci_2 = pi_p[i]*(pci_1 - pci_2) - phi_p[i]*pci_2 - kappa_p[i]*pci_2 - cp_2,
      pci_3 = pi_p[i]*(pci_2 - pci_3) - phi_p[i]*pci_3 - kappa_p[i]*pci_3 - cp_3,
      pci_4 = pi_p[i]*(pci_3 - pci_4) - phi_p[i]*pci_4 - kappa_p[i]*pci_4 - cp_4,
      pci_5 = pi_p[i]*(pci_4 - pci_5) - phi_p[i]*pci_5 - kappa_p[i]*pci_5 - cp_5,
      pn_1  = pi_p[i]*f_a,
      pn_2  = pi_p[i]*pci_1,
      pn_3  = pi_p[i]*pci_2,
      pn_4  = pi_p[i]*pci_3,
      pn_5  = pi_p[i]*pci_4,
      ftl_p = phi_p[i]*(f_a + pci_1 + pci_2 + pci_3 + pci_4 + pci_5),
      ftl_m = phi_m[i]*(g_a + m),
      pci_t = pn_1+pn_2+pn_3+pn_4+pn_5+pi_m[i]*(g_a+m),
       
      disc  = -discount*disc
      ),
      f     = f_a,
      g     = g_a,
      prime = f_a + pci_1 + pci_2 + pci_3 + pci_4 + pci_5,
      maint = g_a + m,
      fatal = ftl_p + ftl_m 
    )
  })
}

yinit <- c(m = 0, cabg=0,
          pci_1=0, pci_2=0, pci_3=0, pci_4=0, pci_5=0,
          pn_1=0,  pn_2=0,  pn_3=0,  pn_4=0, pn_5=0,
          ftl_p=0, ftl_m=0, pci_t=0, disc=1)
ages  <- seq(0, 10, by=1/365)
system.time(out <- dede(yinit, ages, clopidogrel, params, atol=1e-12))
plot(out)
