days_till_A<- function(attrs, inputs)
{
  # Relative Risk
  rr <- if(attrs[["aTreat"]]==1) inputs$vRR else 1.0
  
  # Baseline Risk
  days <- 365*inputs$vDurationA
  
  # Convert To Probability 
  rate <- (- (log ( 1 - inputs$vRiskA)*rr) / days)
  
  t2e <- rexp(1, rate)
  
  #if(t2e > days || attrs[["eventA"]] != 0) t2e <- 365*inputs$vHorizon + 1
  
  return(t2e)
} 

event_A = function(traj, inputs) 
{
  traj %>% 
  branch(
  function(attrs) sample(1:2,1,prob=c(0.05,0.95)),
  continue = c(FALSE, TRUE),
  trajectory("Die")  %>% release("time_in_model") %>% mark("A_death") %>% mark("A"),
  trajectory("Survive")  %>%  mark("A_survive") %>% mark("A") %>%
    set_attribute("eventA",1) %>% #record occurance of A
    set_attribute("aRR_B",1) %>% #turn on B and adjust clock
    set_attribute("attB", function(attrs) now() + days_till_B(attrs,inputs))
  )
}

days_till_B <- function(attrs,inputs) 
{
  # Baseline Risk
  days <- 365*inputs$vDurationB
  
  # Convert To Probability 
  rate <- (- (log ( 1 - inputs$vRiskB)) / days)
  
  t2e <- rexp(1, rate)
  
  if(t2e > days || attrs[["eventA"]] != 1 || attrs[["eventB"]] != 0)
    t2e <- 365*inputs$vHorizon + 1

  return(t2e)
}

event_B = function(traj, inputs) 
{
  traj %>%
  set_attribute("aRR_B",epsilon) %>%
  mark("B") %>%
  set_attribute("eventB", 1)
}



