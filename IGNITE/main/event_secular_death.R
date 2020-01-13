## Secular Death, Weibull Model
#source('./main/age-weibull.R')
source('./main/age-gompertz.R')

# Given attributes of a patient (trajectory), it returns in days 
# how long till the patient would die a secular death.
#
days_till_death <- function(inputs)
{
      rates = inputs$clopidogrel$vRiskDeath
      rr = 1
      days = 365

    # Convert To instantaneous 
    rates2 = (- (log ( 1 - rates)*rr) / days)
    
    times  <- c(0,365) - ageOfTherapy
    
    timeDeath = rpexp(1, rate=rates2, t=times)
    return(timeDeath)
  
  
}

# Given a trajectory, modify as needed when a secular
# death occurs.
#
# In this case, it marks a counter and terminates 
# the trajectory. A branch is required, even though
# it doesn't branch to force the termination.
secular_death <- function(traj, inputs)
{
  traj %>% branch(
    function() 1,
    continue=c(FALSE), # False is patient death, had to use a branch to force termination
    trajectory("Secular Death") %>% mark("secular_death") %>% cleanup_on_termination()
  )
}