library(simmer)

# Past end of life
past_end_of_life <- 365*120

switch_statin <- function()
{
  create_trajectory("Switch Statin") %>% 
    branch(
      function(attrs) attrs[["CVDdrug"]]+1,
      merge=rep(TRUE,5),
      create_trajectory() %>% timeout(0), # Already no treatment
      create_trajectory("Switch to Second Line") %>%
        mark("switched") %>%
        release("drug1") %>%
        set_attribute("CVDdrug", function(attrs) attrs[['second_line']]) %>%
        branch(
          function(attrs) attrs[['second_line']]-1,
          merge=rep(TRUE,3),
          create_trajectory() %>% seize("drug2"),
          create_trajectory() %>% seize("drug3"),
          create_trajectory() %>% seize("drug4")
        ),
      create_trajectory("Switch to Low Dose") %>%
        mark("switched") %>%
        release("drug2") %>%
        set_attribute("CVDdrug", 4) %>%
        seize("drug4"),
      create_trajectory("Switch to Low Dose") %>%
        mark("switched") %>%
        release("drug3") %>%
        set_attribute("CVDdrug", 4) %>%
        seize("drug4"),
      create_trajectory("Stopped Treatment") %>%
        mark("stopped")  %>%
        release("drug4") %>%
        set_attribute("CVDdrug", 0)
  )
}

stop_cvd_treatment <- function()
{
  create_trajectory("Stop CVD Treatment") %>%
    branch(
      function(attrs) (attrs[["CVDdrug"]] == 0) + 1,
      merge=c(TRUE,TRUE),
      create_trajectory("Stopping") %>% mark("stopped") %>% stop_treatment(),
      create_trajectory() %>% timeout(0) # Already no treatment
    )
}

decrease_statin_dose <- function()
{
  create_trajectory("Decrease Statin Dose") %>%
    branch(
      function(attrs){
        drug <- attrs[['CVDdrug']]
        if(drug == 0) {return(1)} else # No treatment now
        if(drug == 4) {return(2)} else # On Low dose currently, -> Stop
        return(3)                      # This is the intent, other TX -> Low Dose
      },
      merge=rep(TRUE,3),
      create_trajectory() %>% timeout(0), # Already no treatment
      create_trajectory("Stopping") %>% mark("stopped") %>% stop_treatment(),
      create_trajectory("Decreasing") %>%
        mark("switched") %>%
        stop_treatment() %>%
        seize("drug4")   %>%
        set_attribute("CVDdrug", 4)
    )
}

next_step <- function(traj)
{
  traj %>%
  branch(
    function() sample(1:3, 1, prob=c(0.591, 0.23, 0.179)),
    merge=rep(TRUE,3),
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
    merge = c(FALSE, TRUE),
    create_trajectory("Severe Myopathy Death") %>% mark("rahbdo_death") %>% cleanup_on_death(),
    create_trajectory("Severe Myopathy")       %>% timeout(0)
  )
}