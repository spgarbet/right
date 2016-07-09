

####################################################################################################################
####################################################################################################################



# This is where the "preemptive" startegy is dealt with

predict_clopidogrel <- function(traj, inputs)
{
  # First, Get the Probability That the Risk Score Returns a Value Above the Threshold
  # TO DO: Fix teh Sensitivity and Specificity to reflect what was done for PREDICT (e.g., 5 years)
  traj %>%
    set_attribute("aPrDAPT.Score.Eq1",  function(attrs)
      inputs$simvastatin$vPREDICTsens * (attrs[['aTimeDAPTInitialized']] <= 365*5) + 
      (1 - inputs$simvastatin$vPREDICTspec) * (1 - (attrs[["aTimeDAPTInitialized"]] < 365*5))) %>%
    set_attribute("aGenotyped_CYP2C19", function(attrs)
        sample(1:2, 1, prob = c(attrs[['aPrDAPT.Score.Eq1']], 1 - attrs[['aPrDAPT.Score.Eq1']])))
}



