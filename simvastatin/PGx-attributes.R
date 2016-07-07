
predict_simvastatin <- function(traj, inputs)
{
  # First, Get the Probability That the Risk Score Returns a Value Above the Threshold
  traj %>%
    set_attribute("aPrCVD.Score.Eq1",  function(attrs)
      inputs$simvastatin$vPREDICTsens * (attrs[['aTimeCVDInitialized']] <= inputs$vHorizon*365) + 
      (1 - inputs$simvastatin$vPREDICTspec) * (1 - (attrs[["aTimeCVDInitialized"]] < inputs$vHorizon*365))) %>%
    set_attribute("aGenotyped_CVD", function(attrs)
        sample(1:2, 1, prob = c(attrs[['aPrCVD.Score.Eq1']], 1 - attrs[['aPrCVD.Score.Eq1']])))
}

assign_cvd_medication <- function(traj, inputs)
{
  traj %>%
  branch(
    function() (is.na(inputs$vPGx) || inputs$vPG=="None")  + 1,
    merge=c(TRUE,TRUE),
    create_trajectory() %>% timeout(0),
    create_trajectory("Genotyped") %>% mark("genotyped")
  ) %>%
  branch(
    function(attrs) {
      # No treatment at all
      if(!inputs$vTX) return(5)
      
      # If there is no genotyping, or it's reactive, default to Simvastatin
      if(is.na(inputs$vPGx) || inputs$vPGx == "Reactive") return(1)
      
      # If the genotype is good metabolizer, then default to Simvastatin
      if(attrs[['CVDgenotype']] == 1) return(1)
      
      # Assign second line
      return(attrs[['second_line']])
    },
    merge=rep(TRUE,5),
    create_trajectory("Simvastatin")  %>%
      set_attribute("CVDdrug", 1) %>%
      seize("drug1"),
    create_trajectory("Atorvastin")   %>%
      set_attribute("CVDdrug", 2) %>%
      seize("drug2") %>%
      mark("switched"),
    create_trajectory("Rosuvastatin") %>%
      set_attribute("CVDdrug", 3) %>%
      seize("drug3") %>%
      mark("switched"),
    create_trajectory("Low/Moderate Dose Statin") %>%
      set_attribute("CVDdrug", 4) %>%
      seize("drug4") %>%
      mark("switched"),
    create_trajectory("No Treatment") %>%
      set_attribute("CVDdrug", 0)
  )
}



