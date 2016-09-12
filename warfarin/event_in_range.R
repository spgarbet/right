
days_till_in_range <- function(attrs, inputs)
{
  switch = attrs[["sWarfarinEvents"]]
  cat = attrs[["aInRange"]]
  PGx =  attrs[["aGenotyped_Warfarin"]]
  st = inputs$vReactive
  tested = attrs[["aPredicted"]]
  
  if (switch==1 & cat==2) #must be on warfarin and currently out of range
  {
    if (PGx==1 & st=="None")                  {t2e <- rexp(1, inputs$warfarin$vMedianTimetoINR_PGx)} #genotype-guided dosing: no delay (not Reactive Strategy)
    else if (st!="None" & tested==1 & PGx==1) {t2e <- rexp(1, inputs$warfarin$vMedianTimetoINR_PGx)} #genotype-guided dosing: no delay (picked up by PREDICT)
    else if (st!="None" & tested==2 & PGx==1) {t2e <- rexp(1, inputs$warfarin$vMedianTimetoINR_PGx_delay)} #genotype-guided dosing: with 3-day delay (picked up by reactive strategy) 
    else if (PGx==2)                          {t2e <- rexp(1, inputs$warfarin$vMedianTimetoINR)} #usual care
  }
  else {t2e <- inputs$vHorizon*365+1} 
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



