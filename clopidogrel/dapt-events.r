
####
## Assign Time to DAPT
days_till_dapt <- function(attrs, inputs) 
{
  aRandUnif = runif(n=1,min=0,max=1) 
  aLPEvent = attrs[['aRRDAPT']]
  inputs$clopidogrel$vDAPTScale * (-log(aRandUnif)*exp(-log(aLPEvent)))^(1/inputs$clopidogrel$vDAPTShape)
}


# # Sanity Check: Should be ~4%
#  x = c()
#  for (i in 1:10000) x[i] = days_till_dapt(1); prop.table(table(x<=365))


######
## Assign DAPT Medication

clopidogrel_reactive_strategy <- function(traj, inputs)
{
  if(inputs$vReactive == "None") 
  {
    traj # Do nothing to trajectory
  } else if (inputs$vReactive == "Single")
  {
    traj %>%
    branch(
      function(attrs) attrs[['aGenotyped_CYP2C19']],
      continue=c(TRUE, TRUE),
      create_trajectory() %>% timeout(0),
      create_trajectory() %>% set_attribute("aGenotyped_CYP2C19", 1) %>% mark("single_test")
    )
  } else if (inputs$vReactive == "Panel")
  {
    traj %>%
    branch(
      function(attrs) all_genotyped(attrs)+1,
      continue=c(TRUE, TRUE),
      create_trajectory() %>% panel_test(), # Not all genotyped, then do it
      create_trajectory() %>% timeout(0)    # Already done, ignore
    )
  } else stop("Unhandled Reactive Clopidogrel Strategy")
}

assign_DAPT_medication <- function(traj,inputs) 
{
  traj %>%
    clopidogrel_reactive_strategy(inputs) %>%
    set_attribute("aDAPT.Rx",0) %>%
    set_attribute("aDAPT.SecondLine",
                  function() {
                    if(inputs$clopidogrel$vDAPT.SecondLine == "Ticagrelor")       {return(2)} else
                    if(inputs$clopidogrel$vDAPT.SecondLine == "Prasugrel")        {return(3)} 
                    # Something went very wrong
                    stop("Invalid Logic in assigning DAPT medication")
                  }) %>%
    branch(
      function(attrs) {
        # The genotyped patients are switched with some probability.  
        if(attrs[['aGenotyped_CYP2C19']]==1 & attrs[['aCYP2C19']] == 1 & attrs[['aDAPT.Rx.Hx']]==0 ) {
          return(sample(c(1,attrs[['aDAPT.SecondLine']]),1,prob=c(1-inputs$clopidogrel$vProbabilityDAPTSwitch,inputs$clopidogrel$vProbabilityDAPTSwitch)))
        } else 
        if (attrs[['aDAPT.Rx.Hx']]!=0) {return(attrs[['aDAPT.Rx.Hx']])} 
        return(1) # Default is to Clopidogrel, hence return 1 if no Hx of alternative drug, or if not switched.  
      },
      continue=rep(TRUE,3),
      create_trajectory("sClopidogrel") %>%
        set_attribute("aDAPT.Rx", 1),  
      create_trajectory("sTicagrelor")  %>%
        set_attribute("aDAPT.Rx", 2) %>%
        mark("dapt_switched"),
      create_trajectory("sPrasugrel") %>%
        set_attribute("aDAPT.Rx", 3) %>%
        mark("dapt_switched")
    ) %>%
    
    # Set DAPT Rx History to Whatever Drug You Were Put On
    set_attribute("aDAPT.Rx.Hx",function(attrs) attrs[['aDAPT.Rx']]) %>%
    
    #Initiate Aspirin if Not Already Started
    #TK Need to Seize Resource Until Death
     branch(
       function(attrs) ifelse(attrs[['aAspirin']]==1 & attrs[["aDAPT.Rx"]] %in% c(1,2,3) ,1,2),
      continue=c(TRUE,TRUE),
      create_trajectory() %>% timeout(0),
      create_trajectory() %>% seize("aspirin") %>% set_attribute("aAspirin",1)
     )
  }

dapt <- function(traj, inputs)
{
  traj %>%
    branch( 
      function(attrs) ifelse(attrs[['aNumDAPT']] < inputs$clopidogrel$vMaxDAPT,1,2),
      continue = c(TRUE,TRUE),
      create_trajectory() %>%  
        mark("dapt_start")  %>% 
        set_attribute("aRRDAPT", inputs$clopidogrel$vRRRepeat.DAPT)  %>% 
        set_attribute("aNumDAPT",function(attrs) attrs[['aNumDAPT']]+1) %>%
        set_attribute("aOnDAPT",1) %>%
        assign_DAPT_medication(inputs) %>%

        ####
        ##
        # Downstream Events Go here
        ##
        ####
        
        # End of Therapy
        set_attribute("aDAPTEnded",function(attrs) now(env) + dapt_end_time(attrs)) %>%
        
        # Stent Thrombosis
        set_attribute("aST",function(attrs) now(env) + time_to_ST(attrs)) %>%
        
        # Myocardial Infarction (Non-Fatal)
        set_attribute("aMI",function(attrs) now(env) + time_to_MI(attrs)) %>%
        
        # Revascularizaiton
        set_attribute("aRV",function(attrs) now(env) + time_to_RV(attrs)) %>%
        
        # Extracranial (TIMI major and nonfatal)
        set_attribute("aExtBleed",function(attrs) now(env) + time_to_ExtBleed(attrs)) %>%
      
        # Intracranial (TIMI major and nonfatal)
        set_attribute("aIntBleed",function(attrs) now(env) + time_to_IntBleed(attrs)) %>%
      
        # TIMI minor
        set_attribute("aTIMIMinor",function(attrs) now(env) + time_to_TIMIMinor(attrs)) %>%
        
        # Fatal Bleed
        set_attribute("aFatalBleed",function(attrs) now(env) + time_to_FatalBleed(attrs))
      ,
      create_trajectory() %>% set_attribute("aRRDAPT",epsilon)
    ) 
}

####
##
# DAPT Treatment Course Ends
##
####
dapt_end_time = function(attrs,inputs) {
   if (attrs[["aOnDAPT"]]==1)
   {
     return( inputs$clopidogrel$vDAPT.Tx.Duration )
   } else
     return(inputs$vHorizon*365 +1)
}

dapt_end <- function(traj,inputs) 
{
  traj %>%
    create_trajectory()  %>% mark("dapt_end") %>% set_attribute("aOnDAPT",2)
}

####
##
# Stent Thrombosis Events
##
####
# From Annals Paper:
# We assumed that all stent thromboses resulted in a myocardial infarction (MI), 
# and 20% were fatal (Appendix Table). Based on expert opinion and a review of the literature, 
# we assumed that 10% (range: 5-15%) of those who survived the episode of ST underwent emergent 
# CABG, and, as a simplifying assumption, the others underwent a repeat PCI with a drug-eluting stent.


time_to_ST = function(attrs,inputs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(inputs$vHorizon*365+1) else
  {
    # Relative Risk
    rr = attrs[["aRR.DAPT.ST"]]
    # Need to add in loss of function and gain of function RRs here too.
    if (attrs[['aCYP2C19']] == 1 & attrs[['aDAPT.Rx']]==2) rr = inputs$clopidogrel$vRR.ST.LOF
    if (attrs[['aDAPT.Rx']]==2) rr = inputs$clopidogrel$vRR.ST.Ticagrelor 
    if (attrs[['aDAPT.Rx']]==3) rr = inputs$clopidogrel$vRR.ST.Prasugrel
    if (attrs[['aDAPT.Rx']]==4) rr = inputs$clopidogrel$vRR.ST.Aspirin
      
    # Baseline Risk
    rates = c(inputs$clopidogrel$vRiskST30,inputs$clopidogrel$vRiskST365,inputs$clopidogrel$vRiskSTgt365)
    days = c(30,365,365*4)
    
    # Convert To Probability 
    rates2 = (- (log ( 1 - rates)*rr) / days)
    
    timeST = rpexp(1, rate=c(rates2,epsilon), t=c(0,days))
    return(timeST)
    
  }
}

ST_event = function(traj, inputs) 
{
  traj %>%
    branch(
      function(attrs)
        ifelse(attrs[["aOnDAPT"]] == 1, 1, 2),
      continue = c(TRUE, TRUE),
    create_trajectory()  %>% mark("st_all") %>%
    # Case Fatatliy
     branch(
       function(attrs) sample(1:2,1,prob=c(inputs$clopidogrel$vSt.Case.Fatality,1-inputs$clopidogrel$vSt.Case.Fatality)),
       continue=c(FALSE,TRUE),
       create_trajectory() %>% mark("st_fatal") %>% cleanup_on_death(),
       create_trajectory() %>% 
         branch(
           function(attrs) sample(1:2,1,prob=c(inputs$clopidogrel$vPrCABG.ST,1-inputs$clopidogrel$vPrCABG.ST)),
           continue= c(TRUE,TRUE),
           # Discontinue DAPT Therapy if CABG, Continue With Aspirin
           create_trajectory() %>% mark("cabg") %>% set_attribute("aOnDAPT",2) %>% set_attribute("aDAPT.Rx",4),
           
           #* TO DO: Add in Brief 14 Day Utility Decrement
           
           # Reset Tx Duration to 1 year if PCI
           create_trajectory() %>%  
             set_attribute("aRRDAPT",inputs$clopidogrel$vRRRepeat.DAPT)  %>% 
             set_attribute("aNumDAPT",function(attrs) attrs[['aNumDAPT']]+1) %>%
             set_attribute("aOnDAPT",1) %>% set_attribute("aDAPTEnded",function(attrs) now(env) + dapt_end_time(attrs)) 
           
           #* TO DO: Add in Brief 7 Day Utility Decrement
         )
     ),
  create_trajectory() %>% timeout(0)
  )
}


####
##
# Myocardial Infarction Events
##
####

#Patients were also at risk for MI unrelated to stent thrombosis at a base-case rate of 3.5%/year
#in the clopidogrel arm (16,19,62). Eight percent of these patients underwent coronary artery bypass 
#grafting (CABG) and 55% underwent a percutaneous coronary intervention (PCI) during the index 
#hospitalization for the nonfatal MI (17,72). Patients who had one or more nonfatal MIs experienced 
#a 30% increase in long-term cardiovascular mortality and recurrent MI (72).

time_to_MI = function(attrs, inputs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(inputs$vHorizon*365+1) else
  {
    # Relative Risk
    rr = attrs[["aRR.DAPT.MI"]]
    if (attrs[['aDAPT.Rx']]==2) rr = inputs$clopidogrel$vRR.MI.Ticagrelor 
    if (attrs[['aDAPT.Rx']]==3) rr = inputs$clopidogrel$vRR.MI.Prasugrel
    if (attrs[['aDAPT.Rx']]==4) rr = inputs$clopidogrel$vRR.MI.Aspirin
    
    # Baseline Risk
    rates = rep(inputs$clopidogrel$vRiskMI, 4)
    days = c(365,365*2,365*3,365*4)
    
    # Convert To Probability 
    rates2 = (- (log ( 1 - rates)*rr) / days)
    
    timeST = rpexp(1, rate=c(rates2,epsilon), t=c(0,days))
  
    return(timeST)
    
  }
}



MI_event = function(traj, inputs)
{
  traj %>%
    branch(
      function(attrs)
        ifelse(attrs[["aOnDAPT"]] == 1, 1, 2),
      continue = c(TRUE, TRUE),
      create_trajectory() %>% mark("mi_nonfatal") %>%
        branch(
          function(attrs)
            sample(
              1:3,
              1,
              prob = c(
                inputs$clopidogrel$vPrCABG.MI,
                inputs$clopidogrel$vPrPCI.MI,
                1 - inputs$clopidogrel$vPrCABG.MI - inputs$clopidogrel$vPrPCI.MI
              )
            ),
          continue = c(TRUE, TRUE, TRUE),
          
          # CABG
          create_trajectory() %>% mark("cabg") %>% set_attribute("aOnDAPT", 2) %>% set_attribute("aDAPT.Rx", 4),
          #* TO DO: Add in Brief 14 Day Utility Decrement
          #* TO DO: Confirm DAPT Therapy shut off with CABG.
          
          # Repeat PCI
          create_trajectory() %>%
            set_attribute("aRRDAPT", inputs$clopidogrel$vRRRepeat.DAPT)  %>%
            set_attribute("aNumDAPT", function(attrs)
              attrs[['aNumDAPT']] + 1) %>%
            set_attribute("aOnDAPT", 1) %>% set_attribute("aDAPTEnded", function(attrs)
              now(env) + dapt_end_time(attrs)) ,
          #* TO DO: Add in Brief 7 Day Utility Decrement
          
          create_trajectory() %>%  mark("mi_med_manage")
        ),
      create_trajectory() %>% timeout(0)
    )
}



####
##
# Revascularization
##
####

# We estimated rates of repeat revascularization after PCI for acute coronary syndrome (ACS) 
# from Medicare claims data (2001-2006; Appendix Table) (57, 58). Because we modeled revascularizations
# related to stent thrombosis and MI separately, we subtracted these from the total observed revascularizations 
# to avoid double counting.

time_to_RV = function(attrs, inputs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(inputs$vHorizon*365+1) else
  {
    # Relative Risk
    rr = attrs[["aRR.DAPT.RV"]]

    # Baseline Risk
    rates = c(inputs$clopidogrel$vRiskRV365,rep( inputs$clopidogrel$vRiskRVgt365,3))
    days = c(365,365*2,365*3,365*4)
    
    # Convert To Probability 
    rates2 = (- (log ( 1 - rates)*rr) / days)
    
    timeRV = rpexp(1, rate=c(rates2,epsilon), t=c(0,days))
    
    return(timeRV)
    
  }
}



RV_event = function(traj, inputs) 
{
  traj %>%
    branch(
      function(attrs)
        ifelse(attrs[["aOnDAPT"]] == 1, 1, 2),
      continue = c(TRUE, TRUE),
    create_trajectory() %>% mark("revascularized") %>%
    branch(
      function(attrs) sample(1:2,1,prob=c(inputs$clopidogrel$vPrCABG.RV,
                                          1-inputs$clopidogrel$vPrCABG.RV)),
      continue= c(TRUE,TRUE),
      
      # CABG
      create_trajectory() %>% mark("cabg") %>% set_attribute("aOnDAPT",2) %>% set_attribute("aDAPT.Rx",4),
      #* TO DO: Add in Brief 14 Day Utility Decrement
      #* TO DO: Confirm DAPT Therapy shut off with CABG. 
      
      # Repeat PCI
      create_trajectory() %>%  
        set_attribute("aRRDAPT",inputs$clopidogrel$vRRRepeat.DAPT)  %>% 
        set_attribute("aNumDAPT",function(attrs) attrs[['aNumDAPT']]+1) %>%
        set_attribute("aOnDAPT",1) %>% set_attribute("aDAPTEnded",function(attrs) now(env) + dapt_end_time(attrs))  
      #* TO DO: Add in Brief 7 Day Utility Decrement

    ),
    create_trajectory() %>% timeout(0)
    )
}


####
##
# Bleeding Events
##
####

# Hemorrhage
# Based on Thrombolysis in Myocardial Infarction (TIMI) criteria (36), bleeds were classified
# as minor or major; major bleeds were further divided into extracranial and intracranial. 
# Half of all non-CABG-related major bleeding observed in the first year occurred in the first month 
# after PCI and the hazard was constant over the remaining duration of drug exposure. We also modeled 
# CABG-related TIMI major bleeds and assumed that all excess CABG-related bleeds were extracranial; 
# these were associated with increased costs and decreased quality-adjusted life years (QALYs), but 
# did not increase perioperative mortality.

time_to_ExtBleed = function(attrs, inputs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(inputs$vHorizon*365+1) else
  {
    # Relative Risk
    rr = attrs[["aRR.DAPT.ExtBleed"]]
    if (attrs[['aDAPT.Rx']]==2) rr = inputs$clopidogrel$vRR.ExtBleed.Ticagrelor 
    if (attrs[['aDAPT.Rx']]==3) rr = inputs$clopidogrel$vRR.ExtBleed.Prasugrel
    if (attrs[['aDAPT.Rx']]==4) rr = inputs$clopidogrel$vRR.ExtBleed.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskExtBleed
    days = c(365)
    
    # Convert To Probability 
    rates2 = (- (log ( 1 - rates)*rr) / days)
    
    timeExtBleed = rpexp(1, rate=c(rates2,epsilon), t=c(0,days))

    return(timeExtBleed)
    
  }
}

ExtBleed_event = function(traj, inputs)
{
  traj %>%
    branch(
      function(attrs)
        ifelse(attrs[["aOnDAPT"]] == 1, 1, 2),
        continue=c(TRUE,TRUE),
      create_trajectory() %>% mark("timi_ext_maj_nonfatal"),
      create_trajectory() %>% timeout(0)
    )  
}


##
# Intracranial (TIMI Major and Non-Fatal)
##
time_to_IntBleed = function(attrs, inputs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(inputs$vHorizon*365+1) else
  {
    # Relative Risk
    rr = attrs[["aRR.DAPT.IntBleed"]]
    if (attrs[['aDAPT.Rx']]==2) rr = inputs$clopidogrel$vRR.IntBleed.Ticagrelor 
    if (attrs[['aDAPT.Rx']]==3) rr = inputs$clopidogrel$vRR.IntBleed.Prasugrel
    if (attrs[['aDAPT.Rx']]==4) rr = inputs$clopidogrel$vRR.IntBleed.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskIntBleed
    days = c(365)
    
    # Convert To Probability 
    rates2 = (- (log ( 1 - rates)*rr) / days)
    
    timeIntBleed = rpexp(1, rate=c(rates2,epsilon), t=c(0,days))
    
    return(timeIntBleed)
    
  }
}

IntBleed_event = function(traj, inputs) 
{
  traj %>% 
    branch(
      function(attrs)
        ifelse(attrs[["aOnDAPT"]] == 1, 1, 2),
      continue=c(TRUE,TRUE),
    create_trajectory() %>% mark("timi_int_maj_nonfatal"),
    create_trajectory() %>% timeout(0)
    )    # Make sure to add this to counters
}

##
# TIMI Minor Bleed
##
time_to_TIMIMinor = function(attrs, inputs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(inputs$vHorizon*365+1) else
  {
    # Relative Risk
    rr = attrs[["aRR.DAPT.TIMIMinor"]]
    if (attrs[['aDAPT.Rx']]==2) rr = inputs$clopidogrel$vRR.TIMIMinor.Ticagrelor 
    if (attrs[['aDAPT.Rx']]==3) rr = inputs$clopidogrel$vRR.TIMIMinor.Prasugrel
    if (attrs[['aDAPT.Rx']]==4) rr = inputs$clopidogrel$vRR.TIMIMinor.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskTIMIMinor
    days = c(365)
    
    # Convert To Probability 
    rates2 = (- (log ( 1 - rates)*rr) / days)
    
    timeTIMIMinor = rpexp(1, rate=c(rates2,epsilon), t=c(0,days))
    
    return(timeTIMIMinor)
    
  }
}

TIMIMinor_event = function(traj, inputs) 
{
  traj %>%  
    branch(
      function(attrs)
        ifelse(attrs[["aOnDAPT"]] == 1, 1, 2),
      continue=c(TRUE,TRUE),
    create_trajectory() %>% mark("timi_min_nonfatal"),
  create_trajectory() %>% timeout(0)
  )    # Make sure to add this to counters
}


##
# Fatal Bleed
##
time_to_FatalBleed = function(attrs, inputs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(inputs$vHorizon*365+1) else
  {
    # Relative Risk
    rr = attrs[["aRR.DAPT.FatalBleed"]]
    if (attrs[['aDAPT.Rx']]==2) rr = inputs$clopidogrel$vRR.FatalBleed.Ticagrelor 
    if (attrs[['aDAPT.Rx']]==3) rr = inputs$clopidogrel$vRR.FatalBleed.Prasugrel
    if (attrs[['aDAPT.Rx']]==4) rr = inputs$clopidogrel$vRR.FatalBleed.Aspirin
    
    # Baseline Risk
    rates = inputs$clopidogrel$vRiskFatalBleed
    days = c(365)
    
    # Convert To Probability 
    rates2 = (- (log ( 1 - rates)*rr) / days)
    
    timeFatalBleed =  rpexp(1, rate=c(rates2,epsilon), t=c(0,days))
    
    return(timeFatalBleed)
    
  }
}

FatalBleed_event = function(traj, inputs) 
{
  traj %>% 
    branch(
    function() 1,
    continue=c(FALSE), # False is patient death
    create_trajectory("Fatal Bleed") %>% mark("fatal_bleed") %>% cleanup_on_death()
  )
}


