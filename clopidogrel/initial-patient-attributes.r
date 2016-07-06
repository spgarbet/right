

assign_initial_clopidogrel_attributes <- function(traj,inputs=list()) 
{
  traj %>%
    seize("n_patients") %>% 
    set_attribute("aAge",runif(1,inputs$vLowerAge,inputs$vUpperAge)) %>%
    set_attribute("aFemale",sample(1:2,1,inputs$vPctFemale )) %>% 
    set_attribute("aAgeInitial",function(attrs) attrs[['aAge']])  %>%

    set_attribute("aAspirin",2) %>%
    set_attribute("aOnDAPT",2) %>%
    set_attribute("aRR.DAPT.ST",1) %>%
    set_attribute("aRR.DAPT.MI",1) %>%
    set_attribute("aRR.DAPT.RV",1) %>%
    set_attribute("aRR.DAPT.ExtBleed",1) %>%
    set_attribute("aRR.DAPT.IntBleed",1) %>%
    set_attribute("aRR.DAPT.TIMIMinor",1) %>%
    set_attribute("aRR.DAPT.FatalBleed",1)
  }



