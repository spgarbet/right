library(simmer)

switch_statin <- function(inputs)
{
  create_trajectory("Switch Statin") %>% 
    branch(
      function(attrs) attrs[["aCVDdrug"]]+1,
      continue=rep(TRUE,3),
      create_trajectory() %>% timeout(0), # Already no treatment, nothing to do
      create_trajectory("Switch to Second Line") %>% # Switch from Simvastatin -> Alternate
        mark("sim_switched") %>%
        release("simvastatin") %>%
        seize("alt_simvastatin") %>%
        set_attribute("aStatinRxHx", 2) %>% # 2nd prescription
        set_attribute("aCVDdrug", 2),
      create_trajectory("Evaluate Alternate Treatment") %>%  # On Alternate
        branch(
          function(attrs) min(attrs[["aStatinRxHx"]], 2), # 1 = 2nd round of alternate, 2+ = Stop
          continue=c(TRUE,TRUE),
          create_trajectory("Continuing Alternate Treatment") %>%
            set_attribute("aStatinRxHx", 2), # 2nd prescription
          # Stop
          create_trajectory("Stopping Statin Treatment") %>%  
            mark("sim_stopped") %>%
            release("alt_simvastatin") %>%
            set_attribute("aCVDdrug", 0)
        )
  )
}

stop_statin_treatment <- function(inputs)
{
  create_trajectory("Stop Statin Treatment") %>%
    branch(
      function(attrs) attrs[["aCVDdrug"]]+1,
      continue=rep(TRUE,3),
      create_trajectory() %>% timeout(0), # Already no treatment
      create_trajectory() %>% mark("sim_stopped")  %>% release("simvastatin"),
      create_trajectory() %>% mark("sim_stopped")  %>% release("alt_simvastatin")
    ) %>%
    set_attribute("aCVDdrug", 0)
}


next_step <- function(traj, inputs, probability_stop)
{
  traj %>%
  branch(
    function() sample(1:2, 1, prob=c(1-probability_stop, probability_stop)), 
    continue=rep(TRUE,2),
    switch_statin(inputs),
    stop_statin_treatment(inputs)
  )
}

# Mild Myopathy events
days_till_mild_myopathy <- function(attrs, inputs)
{
  sim  <- inputs$simvastatin
  drug <- attrs[["aCVDdrug"]]
  geno <- attrs[["aCVDgenotype"]]

  time_frame <- 1825 # 5 Years in days
  risk       <- if     (drug == 0) sim$vMildMyoBaseNoVar
                else if(drug == 1) sim$vMildMyoSimNoVar
                else if(drug == 2) sim$vMildMyoAltNoVar

  rr         <- if      (geno == 1) 1
                else if (drug == 0) 1
                else if (geno == 2 && drug == 1) sim$vMildMyoSimMedVar
                else if (geno == 2 && drug == 2) sim$vMildMyoAltMedVar
                else if (geno == 3 && drug == 1) sim$vMildMyoSimPoorVar
                else if (geno == 3 && drug == 2) sim$vMildMyoAltPoorVar 
                else stop("Unhandled mild myopathy geno/drug combination")

  rate       <- -log(1-risk)*rr/time_frame
  t2e <- rexp(1, rate)

  # NOTE: Events are considered to only be in the first year. (but odds were for 5!?)
  if(t2e > 365) {return(inputs$vHorizon*365+1)}

  return(t2e)
}

# Mark a mild myopathy event
mild_myopathy <- function(traj, inputs)
{
  traj %>%
  mark("mild_myopathy") %>%
  next_step(inputs, inputs$simvastatin$vProbSimStopMild)
}

# Moderate myopathy events
days_till_mod_myopathy <- function(attrs, inputs)
{
  sim  <- inputs$simvastatin
  drug <- attrs[["aCVDdrug"]]
  geno <- attrs[["aCVDgenotype"]]

  time_frame <- 1825 # 5 Years in days
  risk       <- if     (drug == 0) sim$vModMyoBaseNoVar
                else if(drug == 1) sim$vModMyoSimNoVar
                else if(drug == 2) sim$vModMyoAltNoVar

  rr         <- if      (geno == 1) 1
                else if (drug == 0) 1
                else if (geno == 2 && drug == 1) sim$vModMyoSimMedVar
                else if (geno == 2 && drug == 2) sim$vModMyoAltMedVar
                else if (geno == 3 && drug == 1) sim$vModMyoSimPoorVar
                else if (geno == 3 && drug == 2) sim$vModMyoAltPoorVar 
                else stop("Unhandled mod myopathy geno/drug combination")

  rate       <- -log(1-risk)*rr/time_frame
  t2e <- rexp(1, rate)

  # NOTE: Events are considered to only be in the first year. (but odds were for 5!?)
  if(t2e > 365) {return(inputs$vHorizon*365+1)}

  return(t2e)
}

# Mark a moderate myopathy event
mod_myopathy <- function(traj,inputs)
{
  traj %>%
  mark("mod_myopathy") %>%
  next_step(inputs, inputs$simvastatin$vProbSimStopMod)
}

# Severe myopathy events
days_till_sev_myopathy <- function(attrs,inputs)
{
  sim  <- inputs$simvastatin
  drug <- attrs[["aCVDdrug"]]
  geno <- attrs[["aCVDgenotype"]]

  time_frame <- 1825 # 5 Years in days
  risk       <- if     (drug == 0) sim$vSevMyoBaseNoVar
                else if(drug == 1) sim$vSevMyoSimNoVar
                else if(drug == 2) sim$vSevMyoAltNoVar

  rr         <- if      (geno == 1) 1
                else if (drug == 0) 1
                else if (geno == 2 && drug == 1) sim$vSevMyoSimMedVar
                else if (geno == 2 && drug == 2) sim$vSevMyoAltMedVar
                else if (geno == 3 && drug == 1) sim$vSevMyoSimPoorVar
                else if (geno == 3 && drug == 2) sim$vSevMyoAltPoorVar 
                else stop("Unhandled severe myopathy geno/drug combination")

  rate       <- -log(1-risk)*rr/time_frame
  t2e <- rexp(1, rate)

  # NOTE: Events are considered to only be in the first year. (but odds were for 5!?)
  if(t2e > 365) {return(inputs$vHorizon*365+1)}

  return(t2e)
}

# Mark a severe myopathy event
sev_myopathy <- function(traj,inputs)
{
  traj %>%
  mark("sev_myopathy") %>%
  branch(
    function() sample(1:2, 1, prob=c(0.1, 0.9)),
    continue = c(FALSE, TRUE),
    create_trajectory("Severe Myopathy Death") %>% mark("rahbdo_death") %>% cleanup_on_termination(),
    create_trajectory("Do we stop treatment?") %>% next_step(inputs, inputs$simvastatin$vProbSimStopSev)
  )
}