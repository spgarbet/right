library(simmer)


switch_statin <- function(inputs)
{
  create_trajectory("Switch Statin") %>% 
    branch(
      function(attrs) attrs[["aCVDdrug"]]+1,
      continue=rep(TRUE,3),
      create_trajectory() %>% timeout(0), # Already no treatment
      create_trajectory("Switch to Second Line") %>%
        mark("sim_switched") %>%
        release("simvastatin") %>%
        seize("alt_simvastatin") %>%
        set_attribute("aCVDdrug", 2),
      create_trajectory("Stopped Treatment") %>%
        mark("stopped") %>%
        release("alt_simvastatin") %>%
        set_attribute("aCVDdrug", 0)
  )
}

stop_cvd_treatment <- function(inputs)
{
  create_trajectory("Stop CVD Treatment") %>%
    branch(
      function(attrs) attrs[["aCVDdrug"]]+1,
      continue=rep(TRUE,3),
      create_trajectory() %>% timeout(0), # Already no treatment
      create_trajectory() %>% mark("sim_stopped")  %>% release("simvastatin"),
      create_trajectory() %>% mark("sim_stopped")  %>% release("alt_simvastatin")
    ) %>%
    set_attribute("aCVDdrug", 0)
}


next_step <- function(traj, inputs)
{
  traj %>%
  branch(
    function() sample(1:2, 1, prob=c(0.77, 0.23)),
    continue=rep(TRUE,2),
    switch_statin(inputs),
    stop_cvd_treatment(inputs)
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
  next_step(inputs)
}

# Moderate myopathy events
days_till_mod_myopathy <- function(attrs, inputs)
{
  drug <- attrs[["aCVDdrug"]]
  gt   <- attrs[["CVDgenotype"]]
  
  rr <- if(drug == 1)
  {
    c(1, 2.55, 9.56)[gt]
  } else if(drug == 2)
  {
    c(1, 1.08, 4.05)[gt]
  } else
  {
    1
  }
  
  time_frame <- 365 # 1 Year
  risk       <- if(drug == 0) 1e-10 else 0.00011
  rate       <- -log(1-risk)*rr/time_frame
  
  t2e <- rexp(1, rate)
  
  # Events are considered to only be in the first year, otherwise beyond end of life
  if(t2e > 365) {return(inputs$vHorizon*365+1)}
  
  return(t2e)
}

# Mark a moderate myopathy event
mod_myopathy <- function(traj,inputs)
{
  traj %>%
  mark("mod_myopathy") %>%
  next_step()
}

# Severe myopathy events
days_till_sev_myopathy <- function(attrs,inputs)
{
  drug <- attrs[["aCVDdrug"]]
  gt   <- attrs[["CVDgenotype"]]
  
  rr <- if(drug == 1)
  {
    c(1, 2.55, 9.56)[gt]
  } else if(drug == 2)
  {
    c(1, 1.08, 4.05)[gt]
  } else
  {
    1
  }
  
  time_frame <- 365 # 1 Year
  risk       <- if(drug == 0) 1e-16 else 0.000034
  rate       <- -log(1-risk)*rr/time_frame
  
  t2e <- rexp(1, rate)
  
  # Events are considered to only be in the first year, otherwise beyond end of life
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
    create_trajectory("Severe Myopathy")       %>% timeout(0)
  )
}