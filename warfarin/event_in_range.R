
days_till_in_range <- function(attrs, inputs)
{
  switch = attrs[["sWarfarinEvents"]]
  cat = attrs[["aInRange"]]
  PGx =  attrs[["aGenotyped_Warfarin"]]
  
  if(switch==1 & cat==2 & PGx==1 )      {t2e <- rexp(1, inputs$warfarin$vMedianTimetoINR_PGx)} #genotype-guided dosing
  else if (switch==1 & cat==2 & PGx==2) {t2e <- rexp(1, inputs$warfarin$vMedianTimetoINR)} #usual care
  else                 {t2e <- inputs$vHorizon*365+1} 
  return(t2e)
}

get_in_range <- function(traj,inputs)
{
  traj %>%
    release("out_of_range") %>%
    seize("in_range")%>%
    set_attribute("aINR", 2.5) %>% 
    set_attribute("aInRange", 1)
}



