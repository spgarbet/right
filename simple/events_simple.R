days_till_A<- function(attrs, inputs)
{
  # Relative Risk
  if(attrs[["aTreat"]]==1) 
    {rr = inputs$vRR*attrs[["aRR_A"]]} else {rr = attrs[["aRR_A"]]}
  
  # Baseline Risk
  rates = 0.1
  days = 365*5
  
  # Convert To Probability 
  rates2 = (- (log ( 1 - rates)*rr) / days)
  
  t2e = rpexp(1, rate=c(rates2,epsilon), t=c(0,days))
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
    set_attribute("aRR_A",epsilon) %>% #turn off A
    set_attribute("aRR_B",1) %>% #turn on B and adjust clock
    set_attribute("attB", function(attrs) now(env) + days_till_B(attrs,inputs))
  )
}

  

days_till_B <- function(attrs,inputs) 
{
  # Relative Risk
  rr = attrs[["aRR_B"]]
  
  # Baseline Risk
  rates = inputs$vRiskB
  days = 365*5
  
  # Convert To Probability 
  rates2 = (- (log ( 1 - rates)*rr) / days)
  
  t2e = rpexp(1, rate=c(rates2,epsilon), t=c(0,days))
  return(t2e)
}

event_B = function(traj, inputs) 
{
  traj %>% set_attribute("aRR_B",epsilon) %>% mark("B")
}



