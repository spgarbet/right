## Keeping this for now, even though it is not used.

#####
## Fixed Parameters
vPctFemale  = 0.5


######

assign_initial_attributes <- function(traj,inputs=list()) 
{
  traj %>%
    seize("aTimeInModel") %>% 
    set_attribute("aAge",sample(40:65,1)) %>%
    set_attribute("aFemale",1) %>% 
    set_attribute("aAgeInitial",function(attrs) attrs[['aAge']])  %>%
    set_attribute("aUtility",1) %>%
    set_attribute("aTotalUtility",0)  %>%
    set_attribute("aTotalCost",0) %>%
    set_attribute("aAspirin",2)
  }



