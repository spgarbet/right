
####
## Assign DAPT-Indication-Related Parameters
assign_dapt_attributes <- function(traj,inputs) 
{
  traj %>%
    set_attribute("aRRDAPT",1) %>%
    set_attribute("aNumDAPT",0) %>%
    set_attribute("aDAPT.Rx.Hx",0)
}

####
## Assign Time to DAPT
days_till_dapt <- function(attrs) 
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

assign_DAPT_medication <- function(traj,inputs) 
{
  traj %>%
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
        # Under the prospective genotyping scenario, the genotyped patients are switched with some probability.  
        if(inputs[["Global"]]$Scenario == "PGx-Prospective" & attrs[['aGenotyped']]==1 & attrs[['aCYP2C19']] == 1 & attrs[['aDAPT.Rx.Hx']]==0 ) {
          return(sample(c(1,attrs[['aDAPT.SecondLine']]),1,prob=c(1-inputs$clopidogrel$vProbabilityDAPTSwitch,inputs$clopidogrel$vProbabilityDAPTSwitch)))
        } else 
        if (attrs[['aDAPT.Rx.Hx']]!=0) {return(attrs[['aDAPT.Rx.Hx']])} 
        return(1) # Default is to Clopidogrel, hence return 1 if no Hx of alternative drug, or if not switched.  
      },
      merge=rep(TRUE,3),
      create_trajectory("sClopidogrel") %>%
        set_attribute("aDAPT.Rx", 1),  
      create_trajectory("sTicagrelor")  %>%
        set_attribute("aDAPT.Rx", 2) %>%
        mark("Switched.DAPT"),
      create_trajectory("sPrasugrel") %>%
        set_attribute("aDAPT.Rx", 3) %>%
        mark("Switched.DAPT")
    ) %>%
    
    # Set DAPT Rx History to Whatever Drug You Were Put On
    set_attribute("aDAPT.Rx.Hx",function(attrs) attrs[['aDAPT.Rx']]) %>%
    
    #Initiate Aspirin if Not Already Started
    #TK Need to Seize Resource Until Death
     branch(
       function(attrs) ifelse(attrs[['aAspirin']]==1 & attrs[["aDAPT.Rx"]] %in% c(1,2,3) ,1,2),
      merge=c(TRUE,TRUE),
      create_trajectory() %>% timeout(0),
      create_trajectory() %>% seize("Aspirin") %>% set_attribute("aAspirin",1)
     )
  }

dapt <- function(traj)
{
  traj %>%
    branch( 
      function(attrs) ifelse(attrs[['aNumDAPT']] < inputs$clopidogrel$vMaxDAPT,1,2),
      merge = c(TRUE,TRUE),
      create_trajectory() %>%  
        mark("DAPT Initiated")  %>% 
        set_attribute("aRRDAPT",inputs$clopidogrel$vRRRepeat.DAPT)  %>% 
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
        set_attribute("aRV",function(attrs) now(env) + time_to_RV(attrs))
        
      ,
      create_trajectory() %>% set_attribute("aRRDAPT",epsilon)
    ) 
}

####
##
# DAPT Treatment Course Ends
##
#
dapt_end_time = function(attrs) {
   if (attrs[["aOnDAPT"]]==1)
   {
     return( inputs$clopidogrel$vDAPT.Tx.Duration )
   } else
     return(end_of_model +1)
}

dapt_end <- function(traj) 
{
  traj %>%
    create_trajectory()  %>% mark("DAPT Ended") %>% set_attribute("aOnDAPT",2)
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


time_to_ST = function(attrs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(end_of_model+1) else
  {
    # Relative Risk
    rr = attrs[["aRR.DAPT.ST"]]
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

ST_event = function(traj) 
{
  traj %>%
    create_trajectory()  %>% mark("Stent Thrombosis") %>%
    # Case Fatatliy
     branch(
       function(attrs) sample(1:2,1,prob=c(inputs$clopidogrel$vSt.Case.Fatality,1-inputs$clopidogrel$vSt.Case.Fatality)),
       merge=c(FALSE,TRUE),
       create_trajectory() %>% mark("ST Case Fatality") %>% cleanup_on_death(),
       create_trajectory() %>% 
         branch(
           function(attrs) sample(1:2,1,prob=c(inputs$clopidogrel$vPrCABG.ST,1-inputs$clopidogrel$vPrCABG.ST)),
           merge= c(TRUE,TRUE),
           # Discontinue DAPT Therapy if CABG, Continue With Aspirin
           create_trajectory() %>% mark("CABG") %>% set_attribute("aOnDAPT",2) %>% set_attribute("aDAPT.Rx",4),
           
           #* TO DO: Add in Brief 14 Day Utility Decrement
           
           # Reset Tx Duration to 1 year if PCI
           create_trajectory() %>%  
             set_attribute("aRRDAPT",inputs$clopidogrel$vRRRepeat.DAPT)  %>% 
             set_attribute("aNumDAPT",function(attrs) attrs[['aNumDAPT']]+1) %>%
             set_attribute("aOnDAPT",1) %>% set_attribute("aDAPTEnded",function(attrs) now(env) + dapt_end_time(attrs)) 
           
           #* TO DO: Add in Brief 7 Day Utility Decrement
         )
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

time_to_MI = function(attrs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(end_of_model+1) else
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



MI_event = function(traj) 
{
  traj %>%  
     create_trajectory() %>% mark("Non Fatal MI") %>%
        branch(
          function(attrs) sample(1:3,1,prob=c(inputs$clopidogrel$vPrCABG.MI,
                                              inputs$clopidogrel$vPrPCI.MI,
                                              1-inputs$clopidogrel$vPrCABG.MI - inputs$clopidogrel$vPrPCI.MI)),
          merge= c(TRUE,TRUE,TRUE),
          
          # CABG
          create_trajectory() %>% mark("CABG") %>% set_attribute("aOnDAPT",2) %>% set_attribute("aDAPT.Rx",4),
            #* TO DO: Add in Brief 14 Day Utility Decrement
            #* TO DO: Confirm DAPT Therapy shut off with CABG. 
          
          # Repeat PCI
          create_trajectory() %>%  
            set_attribute("aRRDAPT",inputs$clopidogrel$vRRRepeat.DAPT)  %>% 
            set_attribute("aNumDAPT",function(attrs) attrs[['aNumDAPT']]+1) %>%
            set_attribute("aOnDAPT",1) %>% set_attribute("aDAPTEnded",function(attrs) now(env) + dapt_end_time(attrs)) , 
            #* TO DO: Add in Brief 7 Day Utility Decrement
          
          create_trajectory() %>%  mark("MI: Medical Management")
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

time_to_RV = function(attrs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(end_of_model+1) else
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



RV_event = function(traj) 
{
  traj %>%  
    create_trajectory() %>% mark("Revascularization") %>%
    branch(
      function(attrs) sample(1:2,1,prob=c(inputs$clopidogrel$vPrCABG.RV,
                                          1-inputs$clopidogrel$vPrCABG.RV)),
      merge= c(TRUE,TRUE),
      
      # CABG
      create_trajectory() %>% mark("CABG") %>% set_attribute("aOnDAPT",2) %>% set_attribute("aDAPT.Rx",4),
      #* TO DO: Add in Brief 14 Day Utility Decrement
      #* TO DO: Confirm DAPT Therapy shut off with CABG. 
      
      # Repeat PCI
      create_trajectory() %>%  
        set_attribute("aRRDAPT",inputs$clopidogrel$vRRRepeat.DAPT)  %>% 
        set_attribute("aNumDAPT",function(attrs) attrs[['aNumDAPT']]+1) %>%
        set_attribute("aOnDAPT",1) %>% set_attribute("aDAPTEnded",function(attrs) now(env) + dapt_end_time(attrs))  
      #* TO DO: Add in Brief 7 Day Utility Decrement

    )
}



