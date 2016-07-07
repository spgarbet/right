  ###########################################################################
 ##
##  Assign and Track Cardiovascular Events
##
## Risk is a composite based on age and gener
## Source: http://chd.bestsciencemedicine.com/calc2.html
## Risks of heart attacks, angina/coronary insufficiency, heart failure, 
## stroke and intermittent claudication based on data from Framingham
## Cohort Study.
##
## Trying to look at replacing the CVD event generator

# This causes a reassessment of CVD risk based on age
days_till_reassess_cvd <- function(attrs,inputs) { 2.5*365 }

reassess_cvd <- function(traj,inputs)
{
  traj # Does nothing, but triggers reassessment, via reactive events
}

age_bracket <- function(attrs)
{
  age <- attrs[['aAge']]
  
  if (age <= 40) { return(1) } else
  if (age <= 45) { return(2) } else
  if (age <= 50) { return(3) } else
  if (age <= 55) { return(4) } else
  if (age <= 60) { return(5) } else
  if (age <= 65) { return(6) } else
  if (age <= 70) { return(7) } else
  if (age <= 75) { return(8) }
  
  return(9)
}

days_till_cvd <- function(attrs, inputs)
{
  # Are they at risk of being prescribed Statins => increased CVD risk
  # QUESTION: Should this be dictated by end of life, not just end of simulation?
  if(attrs[['aStartStatin']] > inputs$vHorizon*365)
  {
    return(inputs$vHorizon*365+ 1) # No CVD present above background rate
  }
  
  drug       <- attrs[['aCVDdrug']]
  gender     <- attrs[['aGender']]
  bracket    <- age_bracket(attrs)
  time_frame <- 3650 # Rates are over 10 years
  probCVDf <- c(0.02071300,
                0.03951050,
                0.05464350,
                0.06501275,
                0.12643400,
                0.15508900,
                0.18232800,
                0.20793550,
                0.23554300)
  probCVDm <- c(0.04010200,
                0.06818350,
                0.09897900,
                0.01326720,
                0.19648750,
                0.23077900,
                0.27363950,
                0.30658550,
                0.34763400)
  
  rr <- if(drug==0) {1} else {0.65}
  
  prob <- if(gender == 1) {probCVDm[bracket]} else {probCVDf[bracket]}
  
  rate <- -log(1-prob)*rr/time_frame
  
  rexp(1, rate)
}

cvd <- function(traj,inputs)
{
  traj %>%
  mark("cvd") %>%
  branch(
    function() sample(1:2, 1, prob=c(0.117, 0.883)),
    continue=c(FALSE, TRUE),
    create_trajectory("CVD w/ Death") %>% mark("cvd_death") %>% cleanup_on_termination(),
    create_trajectory("CVD Event")    %>% timeout(0)
  ) %>%
  assign_statin(inputs)
}