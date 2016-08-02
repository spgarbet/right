days_till_stroke <- function(attrs, inputs)
{ 
  switch = attrs[["sWarfarinEvents"]]
  if(switch==1) {
    x = attrs[["aINR"]]
    if(attrs[["aWarfarinIndication"]]==1) #AF
    { 
      if(x<1.5)                return(t2e_rexp(inputs$warfarin$vAF_Risk_Stroke_1.5,inputs$warfarin$vRRStroke_AF,inputs$warfarin$vTimeDurStroke))
      else if(x>=1.5 & x<2)   return(t2e_rexp(inputs$warfarin$vAF_Risk_Stroke_1.5to2,inputs$warfarin$vRRStroke_AF,inputs$warfarin$vTimeDurStroke))
      else                   return(t2e_rexp(inputs$warfarin$vAF_Risk_Stroke_Over2,inputs$warfarin$vRRStroke_AF,inputs$warfarin$vTimeDurStroke))
    }
    else #Non-AF
    { 
      if(x<3)                return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Stroke_3,inputs$warfarin$vRRStroke_NonAF,inputs$warfarin$vTimeDurStroke))
      else                   return(t2e_rexp(inputs$warfarin$vNonAF_Risk_Stroke_Over3,inputs$warfarin$vRRStroke_NonAF,inputs$warfarin$vTimeDurStroke))
    }
  }
  else {return(inputs$vHorizon*365+1)}
}

vStroke_2_freq <- c(inputs$warfarin$vR_Stroke_MinorDeficit_2, inputs$warfarin$vR_Stroke_MajorDeficit_2, inputs$warfarin$vR_Stroke_Fatal_2)
vStroke_Over2_freq <- c(inputs$warfarin$vR_Stroke_MinorDeficit_Over2, inputs$warfarin$vR_Stroke_MajorDeficit_Over2, inputs$warfarin$vR_Stroke_Fatal_Over2)

stroke_event <- function(traj, inputs)
{
  traj %>%
    branch(
      function(attrs) {
        if(attrs[["aINR"]]<2) return(1)
        else                  return(2)
      },
      continue=rep(TRUE,2),
      create_trajectory("INR < 2") %>%
        branch(
          function() sample(1:3, 1, prob=vStroke_2_freq),
          continue=c(TRUE,TRUE,FALSE),
          create_trajectory("minor deficit") %>% 
            set_attribute("aTypeofStroke", 1) %>% mark("Stroke_MinorDeficit"),
          
          # back to main model (at risk for other drugs, add prescrition redraw later )
          create_trajectory("major deficit") %>%
            set_attribute("aTypeofStroke", 2) %>% mark("Stroke_MajorDeficit") %>% mark("pass_stroke_switch") %>% 
            set_attribute("sWarfarinEvents", 2) %>% #switch off
            cleanup_warfarin() %>% adj_clock(),
          
          create_trajectory("fatal") %>%
            set_attribute("aTypeofStroke", 3) %>% mark("Stroke_Fatal") %>% cleanup_on_termination()
        ),
      create_trajectory("INR >=2")%>%
        branch(
          function() sample(1:3, 1, prob=vStroke_Over2_freq),
          continue=c(TRUE,TRUE,FALSE),
          create_trajectory("minor deficit") %>%
            set_attribute("aTypeofStroke", 1) %>% mark("Stroke_MinorDeficit"),
          
          # back to main model (at risk for other drugs, add prescrition redraw later )          
          create_trajectory("major deficit") %>%
            set_attribute("aTypeofStroke", 2) %>% mark("Stroke_MajorDeficit") %>% mark("pass_stroke_switch") %>%
            set_attribute("sWarfarinEvents", 2) %>% #switch off
            cleanup_warfarin() %>% adj_clock() ,
          
          create_trajectory("fatal") %>%
            set_attribute("aTypeofStroke", 3) %>% mark("Stroke_Fatal") %>% cleanup_on_termination()
        )
    )
}

