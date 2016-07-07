
assign_simvastatin_attributes <- function(traj, inputs)
{
  # Assign Genotype
  traj %>%
  branch(
    function() sample(1:3, 1, prob=c(1-inputs$simvastatin$vMedMetabolizer-inputs$simvastatin$vPoorMetabolizer, 
                                     inputs$simvastatin$vMedMetabolizer,
                                     inputs$simvastatin$vPoorMetabolizer)),
    continue=rep(TRUE,3),
    create_trajectory("TT") %>% set_attribute("CVDgenotype", 1),
    create_trajectory("TC") %>% set_attribute("CVDgenotype", 2),
    create_trajectory("CC") %>% set_attribute("CVDgenotype", 3)
  ) %>%
  set_attribute("aCVDdrug", 0) %>%   # Not prescribed anything
  set_attribute("aGenotyped_CVD", 2) # Initially not genotyped

}
