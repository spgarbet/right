######
## This sub-routine assigns patient genotyping status (Source: PREDICT Prognositic Model)
vSensitivityPrDAPT = 0.74
vSpecificityPrDAPT = 0.61

#####
## CYP2C19 Parmeters (Source: Annals Paper)
vCYP2C19.Poor = 0.21
vCYP2C19.Rapid = 0.33
vCYP2C19.Unknown = 0.07
vCYP2C19.Probs = c(vCYP2C19.Poor,vCYP2C19.Rapid,vCYP2C19.Unknown,1-vCYP2C19.Poor-vCYP2C19.Rapid-vCYP2C19.Unknown )

# The Probability of Being Genotyped (given a risk score above the threshold is 100%), unless the scenario has genotyping turned off.
if (inputs$Scenario=="PGx-Prospective") vPrGenotyped = 1 else
  vPrGenotyped=0

assign_PGx_attributes <- function(traj,inputs=list()) 
{
  traj %>%
    # First, Get the Probaiblity That the Risk Score Returns a Value Above the Threshold
    set_attribute("aPrDAPT.Score.Eq1",  function(attrs) vSensitivityPrDAPT * (attrs[['eDAPTInitialized']]<=3650) + (1-vSpecificityPrDAPT)*(1-(attrs[["eDAPTInitialized"]]<3650))) %>%
    
    # If this is a Prospective Genotyping Arm, then the person is genotyped with some probability
    branch(
      function() sample(1:2, 1, prob=c(vPrGenotyped,1-vPrGenotyped)),
        merge=c(TRUE, TRUE),
        create_trajectory() %>% set_attribute("aGenotyped",function(attrs) sample(1:2,1,prob=c(attrs[['aPrDAPT.Score.Eq1']],1-attrs[['aPrDAPT.Score.Eq1']]))),
        create_trajectory() %>% set_attribute("aGenotyped",2)
    )  %>%
    branch(
      function(attrs) ifelse(attrs[['aGenotyped']]==1,1,2),
      merge=c(TRUE,TRUE),
      create_trajectory() %>% mark("Number Genotyped"),
      create_trajectory() %>% timeout(0)
    )
    
}

assign_CYP2C19_status <- function(traj,inputs=list())
{
  traj %>%
    branch(
    function() sample(1:4,1,prob=vCYP2C19.Probs),
    merge= rep(TRUE,4),
    create_trajectory()  %>% set_attribute("aCYP2C19",1),
    create_trajectory() %>% set_attribute("aCYP2C19",2),
    create_trajectory()  %>% set_attribute("aCYP2C19",3),
    create_trajectory()  %>% set_attribute("aCYP2C19",4)
  )
}


