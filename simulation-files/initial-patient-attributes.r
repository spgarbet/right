## Keeping this for now, even though it is not used.




######

assign_initial_attributes <- function(traj,inputs=list()) 
{
  traj %>%
    seize("aTimeInModel") %>% 
    set_attribute("aAge",inputs[["Attr"]]$vAge) %>%
    set_attribute("aFemale",sample(1:2,1,inputs[["Attr"]]$vPctFemale )) %>% 
    set_attribute("aAgeInitial",function(attrs) attrs[['aAge']])  %>%
    set_attribute("aUtility",1) %>%
    set_attribute("aTotalUtility",0)  %>%
    set_attribute("aTotalCost",0) %>%
    
    set_attribute("aAspirin",2) %>%
    set_attribute("aOnDAPT",2) %>%
    set_attribute("aRR.DAPT.ST",1)
  }



