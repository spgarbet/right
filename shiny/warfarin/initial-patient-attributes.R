set_INR_seed <- function() {sample(seq(inputs$vN), 1, replace=TRUE)}

assign_initial_INR <- function(traj,inputs)
{
  traj %>%
    set_attribute("aINR", 0) %>%
    set_attribute("aINRInitial", 0) %>% 
    set_attribute("aInRange", 2) %>%
    set_attribute("aSeed", function() set_INR_seed()) %>%
    set_attribute("aOnWarfarin", 2) %>% # not on warfarin yet
    set_attribute("aWarfarinIndication", 1) # not on warfarin yet, first set as 1
}

assign_initial_switch <- function(traj,inputs)
{
  traj %>%
    set_attribute("sWarfarinEvents", 2) %>%  # warfarin events, switch: off
    set_attribute("sINRMonitor", 2) %>%      # monitor INR range, switch: off
    set_attribute("aGenotyped_Warfarin", 2) %>% # initially not genotyped, then depends on PREDICT
    set_attribute("aOrdered_test", 1) %>% # whether order test this time, 1 - NO, 2 - YES
    set_attribute("aReadWarfarinTest", 1) %>% # whether use test result if available, 1 - YES, 2 - NO
    set_attribute("aWTestAvail", 2) #whether test result available at time of prescription, 1 - YES, NO - 2
}  

#wrap
assign_warfarin_attributes <- function(traj, inputs)
{
  traj %>%
    assign_initial_INR(inputs) %>%
    assign_initial_switch(inputs) 
}