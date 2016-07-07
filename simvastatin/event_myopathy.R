library(simmer)

# FIXME FIXME FIXME
# Past end of life
past_end_of_life <- 365*120

switch_statin <- function()
{
  create_trajectory("Switch Statin") %>% 
    branch(
      function(attrs) attrs[["CVDdrug"]]+1,
      continue=rep(TRUE,4),
      create_trajectory() %>% timeout(0), # Already no treatment
      create_trajectory("Switch to Second Line") %>%
        mark("sim_switched") %>%
        release("simvastatin") %>%
        seize("alt_simvastatin") %>%
        set_attribute("CVDdrug", 2),
      create_trajectory("Switch to Low Dose") %>%
        mark("switched") %>%
        release("alt_simvastatin") %>%
        seize("reduced_simvastatin") %>%
        set_attribute("CVDdrug", 3) %>%
      create_trajectory("Stopped Treatment") %>%
        mark("stopped")  %>%
        release("reduced_simvastatin") %>%
        set_attribute("CVDdrug", 0)
  )
}

stop_cvd_treatment <- function()
{
  create_trajectory("Stop CVD Treatment") %>%
    branch(
      function(attrs) attrs[["CVDdrug"]]+1,
      continue=rep(TRUE,4),
      create_trajectory() %>% timeout(0), # Already no treatment
      create_trajectory() %>% mark("sim_stopped")  %>% release("simvastatin"),
      create_trajectory() %>% mark("sim_stopped")  %>% release("alt_simvastatin"),
      create_trajectory() %>% mark("sim_stopped")  %>% release("reduced_simvastatin")
    ) %>%
    set_attribute("CVDdrug", 0)
}

decrease_statin_dose <- function()
{
  create_trajectory("Decrease Statin Dose") %>%
    branch(
      function(attrs){
        drug <- attrs[['CVDdrug']]
        if(drug == 0) {return(1)} else # No treatment now
        if(drug == 3) {return(2)} else # On Low dose currently, -> Stop
        return(3)                      # This is the intent, other TX -> Low Dose
      },
      continue=rep(TRUE,3),
      create_trajectory() %>% timeout(0), # Already no treatment
      stop_cvd_treatment(),
      create_trajectory("Decreasing") %>%
        mark("sim_switched")           %>%
        stop_cvd_treatment()           %>%
        seize("reduced_simvastatin")   %>%
        set_attribute("CVDdrug", 3)
    )
}

next_step <- function(traj)
{
  traj %>%
  branch(
    function() sample(1:3, 1, prob=c(0.591, 0.23, 0.179)),
    continue=rep(TRUE,3),
    switch_statin(),
    stop_cvd_treatment(),
    decrease_statin_dose()
  )
}

# Mild Myopathy events
days_till_mild_myopathy <- function(attrs)
{
  drug <- attrs[["CVDdrug"]]
  
  time_frame <- 1825 # 5 Years
  risk       <- if(drug == 0) 1e-7 else 0.05
  rate       <- -log(1-risk)/time_frame
  
  t2e <- rexp(1, rate)
  
  # Events are considered to only be in the first year, otherwise beyond end of life
  if(t2e > 365) {return(past_end_of_life)}
  
  return(t2e)
}

# Mark a mild myopathy event
mild_myopathy <- function(traj)
{
  traj %>%
  mark("mild_myopathy") %>%
  next_step()
}

# Moderate myopathy events
days_till_mod_myopathy <- function(attrs)
{
  drug <- attrs[["CVDdrug"]]
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
  if(t2e > 365) {return(past_end_of_life)}
  
  return(t2e)
}

# Mark a moderate myopathy event
mod_myopathy <- function(traj)
{
  traj %>%
  mark("mod_myopathy") %>%
  next_step()
}

# Severe myopathy events
days_till_sev_myopathy <- function(attrs)
{
  drug <- attrs[["CVDdrug"]]
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
  if(t2e > 365) {return(past_end_of_life)}
  
  return(t2e)
}

# Mark a severe myopathy event
sev_myopathy <- function(traj)
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