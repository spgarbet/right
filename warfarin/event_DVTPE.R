days_till_DVTPE <- function(attrs, inputs)
{ 
  switch = attrs[["sWarfarinEvents"]]
  if(switch==1) {
    x = attrs[["aINR"]]
    if(attrs[["aWarfarinIndication"]]==1) #AF
    { 
      if(x<3)                return(t2e_rexp(inputs$warfarin$vAF_Risk_DVTPE_3,inputs$warfarin$vRRDVTPE_AF,inputs$warfarin$vTimeDurDVTPE))
      else                   return(t2e_rexp(inputs$warfarin$vAF_Risk_DVTPE_Over3,inputs$warfarin$vRRDVTPE_AF,inputs$warfarin$vTimeDurDVTPE))
    }
    else #Non-AF
    { 
      if(x<3)                return(t2e_rexp(inputs$warfarin$vNonAF_Risk_DVTPE_3,inputs$warfarin$vRRDVTPE_NonAF,inputs$warfarin$vTimeDurDVTPE))
      else                   return(t2e_rexp(inputs$warfarin$vNonAF_Risk_DVTPE_Over3,inputs$warfarin$vRRDVTPE_NonAF,inputs$warfarin$vTimeDurDVTPE))
    }
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
        mark("DVT") %>% 
        set_attribute("aRand01Switch", function() runif(1)),
      create_trajectory("PE") %>% set_attribute("aTypeofDVTPE", 2) %>%
        mark("PE") %>%  set_attribute("aRand01Switch", function() runif(1)),
      create_trajectory("DVTPE_Fatal") %>% 
        set_attribute("aTypeofDVTPE", 3) %>% 
        mark("DVTPE_Fatal") %>% cleanup_on_termination()
    ) %>%
    
    # subjects who experience non-fatal DVTPE randomly switch drug, sent back to main model (at risk for other drugs, add redraw later)
    branch(
      function(attrs) switch_drug(attrs[["aRand01Switch"]]),
      continue=rep(TRUE,2),
      create_trajectory("switch") %>% mark("pass_DVTPE_switch") %>%
        set_attribute("sWarfarinEvents", 2) %>% #switch off
        cleanup_warfarin(),
      create_trajectory("") %>% timeout(0)
    )
}

