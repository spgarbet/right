assign_warfarin_indication <- function(traj, inputs) 
{
  traj %>%
    set_attribute("aTimetoWarfarin_AF", function() rweibull(1,inputs$warfarin$vshape_timetowarfarinAF,inputs$warfarin$vscale_timetowarfarinAF)) %>% 
    set_attribute("aTimetoWarfarin_NonAF", function() rweibull(1,inputs$warfarin$vshape_timetowarfarinAF,inputs$warfarin$vscale_timetowarfarin_nonAF)) %>%
    branch(
      function(attrs) {
        if(attrs[["aTimetoWarfarin_AF"]] < attrs[["aTimetoWarfarin_NonAF"]]) {return(1)}
        else                                                                 {return(2)}},
      continue=rep(TRUE,2),
      create_trajectory("AF") %>% set_attribute("aWarfarinIndication", 1),
      create_trajectory("Non-AF") %>% set_attribute("aWarfarinIndication", 2)
    ) %>%
    set_attribute("aOnWarfarin", 2) # not on warfarin yet
}

assign_initial_INR <- function(traj, inputs) # INR attributes only used upon start of warfarin
{
  traj %>%
    set_attribute("aINRInitial", 
                  function() sample(inputs$warfarin$vINRvalue, 1, prob=inputs$warfarin$vINRfreq)) %>%
    set_attribute("aINR", function(attrs) attrs[["aINRInitial"]]) %>%
    set_attribute("aInRange", function(attrs) INR_status(attrs[["aINRInitial"]]))
}

INR_status <- function(x) {
  if (x>=2 & x<= 3) {return(1)} 
  else {return(2)}
}  

assign_initial_switch <- function(traj)
{
  traj %>%
    set_attribute("sWarfarinEvents", 2) %>%  # warfarin events, switch: off
    set_attribute("sINRMonitor", 2) %>%      # monitor INR range, switch: off
    set_attribute("aGenotyped_Warfarin", 2)  # initially not genotyped, then depends on PREDICT 
}  

#wrap
assign_warfarin_attributes <- function(traj, inputs)
{
  traj %>%
    assign_warfarin_indication(inputs) %>%
    assign_initial_INR(inputs) %>%
    assign_initial_switch()
}