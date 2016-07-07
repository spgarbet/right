counters.dapt <- c(
              "dapt_start",
              "dapt_end",
              "dapt_switched",
              "aspirin",
              "st_all",
              "st_fatal",
              "cabg",
              "mi_nonfatal",
              "mi_med_manage",
              "revascularized",
              "timi_ext_maj_nonfatal",
              "timi_int_maj_nonfatal",
              "timi_min_nonfatal",
              "fatal_bleed"
)

assign_initial_clopidogrel_attributes <- function(traj,inputs) 
{
  traj %>%
    set_attribute("aAspirin",2) %>%
    set_attribute("aOnDAPT",2) %>%
    set_attribute("aRR.DAPT.ST",1) %>%
    set_attribute("aRR.DAPT.MI",1) %>%
    set_attribute("aRR.DAPT.RV",1) %>%
    set_attribute("aRR.DAPT.ExtBleed",1) %>%
    set_attribute("aRR.DAPT.IntBleed",1) %>%
    set_attribute("aRR.DAPT.TIMIMinor",1) %>%
    set_attribute("aRR.DAPT.FatalBleed",1)
}

assign_dapt_attributes <- function(traj,inputs=list()) 
{
  traj %>%
    set_attribute("aRRDAPT",1) %>%
    set_attribute("aNumDAPT",0) %>%
    set_attribute("aDAPT.Rx.Hx",0)
}

# These are the probabilitie of being a poor, rapid, uknown, or normal metabolizer. Just arranging into a vector to facilitate sampling below. 
assign_CYP2C19_status <- function(traj,inputs)
{
  vCYP2C19.Probs = c(inputs$clopidogrel$vCYP2C19.Poor,
                     inputs$clopidogrel$vCYP2C19.Rapid,
                     inputs$clopidogrel$vCYP2C19.Unknown,
                     1-inputs$clopidogrel$vCYP2C19.Poor-inputs$clopidogrel$vCYP2C19.Rapid-inputs$clopidogrel$vCYP2C19.Unknown )
  
  traj %>%
    branch(
    function() sample(1:4,1,prob=vCYP2C19.Probs),
    continue= rep(TRUE,4),
    create_trajectory() %>% set_attribute("aCYP2C19",1), # Poor (currently only one used)
    create_trajectory() %>% set_attribute("aCYP2C19",2), # Rapid
    create_trajectory() %>% set_attribute("aCYP2C19",3), # Unknown (heterozygous)
    create_trajectory() %>% set_attribute("aCYP2C19",4)  # Wildtype
  )
}

assign_clopidogrel_attributes <- function(traj, inputs)
{
  traj %>%
    assign_initial_clopidogrel_attributes(inputs) %>%
    assign_dapt_attributes(inputs) %>%
    assign_CYP2C19_status(inputs)
}
