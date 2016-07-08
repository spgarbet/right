
predict_simvastatin <- function(traj, inputs)
{
  # First, Get the Probability That the Risk Score Returns a Value Above the Threshold
  traj %>%
    set_attribute("aPrCVD.Score.Eq1",  function(attrs)
      inputs$simvastatin$vPREDICTsens * (attrs[['aStartStatin']] <= inputs$vHorizon*365) + 
      (1 - inputs$simvastatin$vPREDICTspec) * (1 - (attrs[["aStartStatin"]] < inputs$vHorizon*365))) %>%
    
    # All this routine needs to do is set the genotyped attribute correctly
    # The main loop code will pick this up and triggers a "panel_test" if needed.
    set_attribute("aGenotyped_CVD", function(attrs)
        sample(1:2, 1, prob = c(attrs[['aPrCVD.Score.Eq1']], 1 - attrs[['aPrCVD.Score.Eq1']])))
}



