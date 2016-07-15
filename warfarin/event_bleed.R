t2e_rexp <- function(risk,rr,time) #used in bleed, stroke, DVTPE
{
  rate = -log(1-risk)*rr/time
  t2e <- rexp(1, rate)
  return(t2e)
}


days_till_bleed <- function(attrs, inputs)
{ 
  switch = attrs[["sWarfarinEvents"]]
  if(switch==1) {
    x = attrs[["aINR"]]
    if(attrs[["aWarfarinIndication"]]==1) #AF
    { 
      if(x<3)                return(t2e_rexp(inputs$warfarin$vAF_Risk_Bleed_3,inputs$warfarin$vRRBleed_AF,inputs$warfarin$vTimeDurBleed))
      else if(x>=3 & x<=4)   return(t2e_rexp(inputs$warfarin$vAF_Risk_Bleed_3to4,inputs$warfarin$vRRBleed_AF,inputs$warfarin$vTimeDurBleed))
      else                   return(t2e_rexp(inputs$warfarin$vAF_Risk_Bleed_Over4,inputs$warfarin$vRRBleed_AF,inputs$warfarin$vTimeDurBleed))
    }
    else #Non-AF
    { 
      if(x<3)                return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Bleed_3,inputs$warfarin$vRRBleed_NonAF,inputs$warfarin$vTimeDurBleed))
      else if(x>=3 & x<=4)   return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Bleed_3to4,inputs$warfarin$vRRBleed_NonAF,inputs$warfarin$vTimeDurBleed))
      else                   return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Bleed_Over4,inputs$warfarin$vRRBleed_NonAF,inputs$warfarin$vTimeDurBleed))
    }
  }
  else {return(inputs$vHorizon*365+1)}
}

vMajorBleedfreq <- c(inputs$warfarin$vR_Bleed_ICH, inputs$warfarin$vR_Bleed_ICH_Fatal, inputs$warfarin$vR_Bleed_GI, 
                     inputs$warfarin$vR_Bleed_GI_Fatal, inputs$warfarin$vR_Bleed_Other, inputs$warfarin$vR_Bleed_Other_Fatal)

bleed_event <- function(traj, inputs)
{
  traj %>%
    branch(
      function() sample(1:2, 1, prob=c(inputs$warfarin$vRisk_MajorBleed, 1-inputs$warfarin$vRisk_MajorBleed)),
      continue=rep(TRUE,2),
      create_trajectory("major bleed") %>% 
        branch(
          function() sample(1:6, 1, prob=vMajorBleedfreq),
          continue=rep(c(TRUE,FALSE),3),
          create_trajectory("ICH") %>% 
            set_attribute("aTypeofBleed",1) %>% mark("MajorBleed_ICH"),
          create_trajectory("ICH_Fatal") %>% 
            set_attribute("aTypeofBleed",2) %>% mark("MajorBleed_ICH_Fatal") %>% cleanup_on_termination(),
          create_trajectory("GI") %>%
            set_attribute("aTypeofBleed",3) %>% mark("MajorBleed_GI"),
          create_trajectory("GI_Fatal") %>%
            set_attribute("aTypeofBleed",4) %>% mark("MajorBleed_GI_Fatal") %>% cleanup_on_termination(),
          create_trajectory("Other") %>%
            set_attribute("aTypeofBleed",5) %>% mark("MajorBleed_Other"),
          create_trajectory("Other_Fatal") %>%
            set_attribute("aTypeofBleed",6) %>% mark("MajorBleed_Other_Fatal") %>% cleanup_on_termination()
        ),
      create_trajectory("minor bleed") %>% mark("MinorBleed")
    )
  
}  




