## Secular Death, Weibull Model
#source('./main/age-weibull.R')
source('./main/age-gompertz.R')

# Given attributes of a patient (trajectory), it returns in days 
# how long till the patient would die a secular death.
#
# NOTE: The variable in must be named attrs
days_till_death <- function(attrs, inputs)
{
  #age       <- attrs[['aAge']]
  #death_age <- ageAtDeath(age, attrs[['aGender']])
  
  #return(365*(death_age-age))
  
  
  if (attrs[["aOnDAPT"]]!=1) return(inputs$vHorizon*365+1) else
  {
    if (attrs[['aCYP2C19']] == 1 & attrs[['aDAPT.Rx']]==1) { #LOF Clopidogrel
      rates = c(inputs$clopidogrel$vRiskDeath30.LOF,inputs$clopidogrel$vRiskDeath365.LOF)
      rr = inputs$clopidogrel$vRR.Death.LOF
    } else if (attrs[['aCYP2C19']] == 1 & attrs[['aDAPT.Rx']]==2) { #LOF Alt
      rates = c(inputs$clopidogrel$vRiskDeath30.Alt.LOF,inputs$clopidogrel$vRiskDeath365.Alt.LOF)
      rr = inputs$clopidogrel$vRR.Death.Alt.LOF
    } else if (attrs[['aCYP2C19']] != 1 & (attrs[['aDAPT.Rx']]==1 | attrs[['aDAPT.Rx']]==2 )) { #Non-LOF
      rates = c(inputs$clopidogrel$vRiskDeath30,inputs$clopidogrel$vRiskDeath365)
      rr = inputs$clopidogrel$vRR.Death
    } else if (attrs[['aDAPT.Rx']]==4) { #Aspirin
      rates = c(epsilon,epsilon)
      rr = c(1,1)
    } else stop("Unhandled ST t2e")    
    
    days = c(30,335)
    
    # Convert To Probability 
    rates2 = (- (log ( 1 - rates)*rr) / days)
    rates2 <- c(rates2, epsilon)
    
    ageOfTherapy <- now(env)
    times  <- c(0,30,365) - ageOfTherapy
    
    if(ageOfTherapy >=30) #redraw will drop the 30-day window
    {
      rates2 <- rates2[2:3]
      times  <- times[2:3]
    } 
    times[1] <- 0
    
    timeDeath = rpexp(1, rate=rates2, t=times)
    return(timeDeath)
    
  }
  
  
  
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