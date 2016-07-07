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
## Method derived from
## D'Agostino, Vasan, Pencina, Wolf, Cobain, Massaro, Kannel,
## "General Cardiovascular Risk Profile for Use in Primary Care: The
##  Framingham Heart Study". Circulation, 2008:117:743-753. 

library(simmer)

days_till_reassess_cvd <- function(attrs) { 3650.0 }

reassess_cvd <- function(traj)
{
  traj %>%
    set_attribute("eCVDTime", function(attrs) now(env)+days_till_cvd(attrs))
}

days_till_cvd <- function(attrs)
{
  drug       <- attrs[['CVDdrug']]
  gender     <- attrs[['gender']]
  age        <- attrs[['age']]
  time_frame <- 3650

  rr <- if(drug==4) {0.75} else
        if(drug==0) {1}    else
                    {0.65}
  
  so10   <- if(gender == 1) { 0.88936} else {0.95012}
  beta   <- if(gender == 1) { 3.06117} else {2.32888}
  center <- if(gender == 1) {11.88213} else {9.06833}
  
  risk <- log(age)*beta - center

  rate <- -log(so10^exp(risk))*rr/time_frame
  
  rexp(1, rate)
}

cvd <- function(traj)
{
  traj %>%
  mark("cvd") %>%
  branch(
    function() sample(1:2, 1, prob=c(0.117, 0.883)),
    merge=c(FALSE, TRUE),
    create_trajectory("CVD w/ Death") %>% mark("cvd_death") %>% cleanup_on_death(),
    create_trajectory("CVD Event") %>% timeout(0)
  )
}