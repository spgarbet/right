

####################################################################################################################
####################################################################################################################



# This is where the "preemptive" startegy is dealt with
assign_clopidogrel_PGx_attributes <- function(traj, inputs = list())
{
  # Note this doesn't have to use branch, because it's a global that every trajectory gets
  if        (inputs$vPreemptive == "None"     )
  {
    traj %>% set_attribute("aGenotyped_CYP2C19", 2)
  } else if (inputs$vPreemptive == "Panel"    )
  {
    traj %>% set_attribute("aGenotyped_CYP2C19", 1) %>% mark("panel_test")
  } else if (inputs$vPreemptive == "PREDICT"  )
  {
# FIXME FIXME FIXME, this should be in main input structure
    # The Probability of Being Genotyped (given a risk score above the threshold is 100%), unless the scenario has genotyping turned off.
    vPrGenotyped = 1 # Probability of genotyping
# FURTHER FIXME what if the panel doesn't include clopidogrel? 
    
    # First, Get the Probaiblity That the Risk Score Returns a Value Above the Threshold
    set_attribute("aPrDAPT.Score.Eq1",  function(attrs)
      inputs$clopidogrel$vSensitivityPrDAPT * (attrs[['aTimeDAPTInitialized']] <=
                                                      3650) + (1 - inputs$clopidogrel$vSpecificityPrDAPT) * (1 - (attrs[["aTimeDAPTInitialized"]] < 3650))) %>%
    # If this is a Prospective Genotyping Arm, then the person is genotyped with some probability
    branch(
      function()
        sample(1:2, 1, prob = c(vPrGenotyped, 1 - vPrGenotyped)),
      continue = c(TRUE, TRUE),
      create_trajectory() %>% set_attribute("aGenotyped_CYP2C19", function(attrs)
        sample(1:2, 1, prob = c(attrs[['aPrDAPT.Score.Eq1']], 1 - attrs[['aPrDAPT.Score.Eq1']]))),
      create_trajectory() %>% set_attribute("aGenotyped_CYP2C19", 2)
    )  %>%
    branch(
      function(attrs)
        ifelse(attrs[['aGenotyped_CYP2C19']] == 1, 1, 2),
      continue = c(TRUE, TRUE),
      create_trajectory() %>% mark("panel_test"),
      create_trajectory() %>% timeout(0)
    )
  } else if (inputs$vPreemptive == "Age >= 50")
  {
    traj %>%
    branch(
      function(attrs) if(attrs[['vAge']] >= 50) 1 else 2,
      continue = c(TRUE, TRUE),
      create_trajectory() %>% set_attribute("aGenotyped_CYP2C19", 2),
      create_trajectory() %>% set_attribute("aGenotyped_CYP2C19", 1) %>% mark("panel_test")
    )
  } else stop("Unhandled Preemptive Clopidogrel Strategy")

}

# These are the probabilitie of being a poor, rapid, uknown, or normal metabolizer. Just arranging into a vector to facilitate sampling below. 
assign_CYP2C19_status <- function(traj,inputs)
{
  vCYP2C19.Probs = c(inputs$clopidogrel$vCYP2C19.Poor,
                     inputs$clopidogrel$vCYP2C19.Rapid,
                     inputs$clopidogrel$vCYP2C19.Unknown,
                     1-inputs$clopidogrel$vCYP2C19.Poor-inputs$clopidogrel$vCYP2C19.Rapid-inputs$clopidogrel$vCYP2C19.Unknown )
  
  traj %>%
    branch(
    function() sample(1:4,1,prob=vCYP2C19.Probs),
    continue= rep(TRUE,4),
    create_trajectory() %>% set_attribute("aCYP2C19",1), # Poor (currently only one used)
    create_trajectory() %>% set_attribute("aCYP2C19",2), # Rapid
    create_trajectory() %>% set_attribute("aCYP2C19",3), # Unknown (heterozygous)
    create_trajectory() %>% set_attribute("aCYP2C19",4)  # Wildtype
  )
}


