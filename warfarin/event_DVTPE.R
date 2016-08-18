days_till_DVTPE <- function(attrs, inputs)
{ 
  switch = attrs[["sWarfarinEvents"]]
  ind = attrs[["aWarfarinIndication"]]
  if(switch==1 & ind==2)  #Non-AF
  { 
    x = attrs[["aINR"]]
    if(x<2)                return(t2e_rexp(inputs$warfarin$vAF_Risk_DVTPE_2,inputs$warfarin$vRRDVTPE_AF,inputs$warfarin$vTimeDurDVTPE))
    else                   return(t2e_rexp(inputs$warfarin$vAF_Risk_DVTPE_Over2,inputs$warfarin$vRRDVTPE_AF,inputs$warfarin$vTimeDurDVTPE))
  }
  else {return(inputs$vHorizon*365+1)} 
}

vDVTPE_freq <- c(inputs$warfarin$vR_DVT, inputs$warfarin$vR_PE, inputs$warfarin$vR_DVTPE_Fatal)

switch_drug <- function(x)
{
  if(x>inputs$warfarin$vPrSwitchDrug) y=1
  else                                y=2
  return(y)
}  

DVTPE_event <- function(traj, inputs)
{
  traj %>%
    branch(
      function() sample(1:3, 1, prob=vDVTPE_freq),
      continue=c(TRUE,TRUE,FALSE),
      create_trajectory("DVT") %>% set_attribute("aTypeofDVTPE", 1) %>%
        mark("DVT"),
      create_trajectory("PE") %>% set_attribute("aTypeofDVTPE", 2) %>%
        mark("PE"),
      create_trajectory("DVTPE_Fatal") %>% 
        set_attribute("aTypeofDVTPE", 3) %>% 
        mark("DVTPE_Fatal") %>% cleanup_on_termination()
    )
}

