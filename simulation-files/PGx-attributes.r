

####################################################################################################################
####################################################################################################################
# The Probability of Being Genotyped (given a risk score above the threshold is 100%), unless the scenario has genotyping turned off.

if (inputs[["Global"]]$Scenario=="PGx-Prospective") vPrGenotyped = 1 else
  vPrGenotyped=0

assign_PGx_attributes <- function(traj,inputs=list()) 
{
  traj %>%
    # First, Get the Probaiblity That the Risk Score Returns a Value Above the Threshold
    set_attribute("aPrDAPT.Score.Eq1",  function(attrs) inputs[["Clopidogrel"]]$vSensitivityPrDAPT * (attrs[['aTimeDAPTInitialized']]<=3650) + (1-inputs[["Clopidogrel"]]$vSpecificityPrDAPT)*(1-(attrs[["aTimeDAPTInitialized"]]<3650))) %>%
    
    # If this is a Prospective Genotyping Arm, then the person is genotyped with some probability
    branch(
      function() sample(1:2, 1, prob=c(vPrGenotyped,1-vPrGenotyped)),
        continue=c(TRUE, TRUE),
        create_trajectory() %>% set_attribute("aGenotyped",function(attrs) sample(1:2,1,prob=c(attrs[['aPrDAPT.Score.Eq1']],1-attrs[['aPrDAPT.Score.Eq1']]))),
        create_trajectory() %>% set_attribute("aGenotyped",2)
    )  %>%
    branch(
      function(attrs) ifelse(attrs[['aGenotyped']]==1,1,2),
      continue=c(TRUE,TRUE),
      create_trajectory() %>% mark("Number Genotyped"),
      create_trajectory() %>% timeout(0)
    )
    
}

assign_CYP2C19_status <- function(traj,inputs=list())
{
  traj %>%
    branch(
    function() sample(1:4,1,prob=inputs[["Clopidogrel"]]$vCYP2C19.Probs),
    continue= rep(TRUE,4),
    create_trajectory()  %>% set_attribute("aCYP2C19",1),
    create_trajectory() %>% set_attribute("aCYP2C19",2),
    create_trajectory()  %>% set_attribute("aCYP2C19",3),
    create_trajectory()  %>% set_attribute("aCYP2C19",4)
  )
}


