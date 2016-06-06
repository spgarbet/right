
####
## Assign DAPT-Indication-Related Parameters
assign_dapt_attributes <- function(traj,inputs=list()) 
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
  inputs[["Clopidogrel"]]$vDAPTScale * (-log(aRandUnif)*exp(-log(aLPEvent)))^(1/inputs[["Clopidogrel"]]$vDAPTShape)
}


# # Sanity Check: Should be ~4%
#  x = c()
#  for (i in 1:10000) x[i] = days_till_dapt(1); prop.table(table(x<=365))


######
## Assign DAPT Medication

assign_DAPT_medication <- function(traj,inputs=list()) 
{
  traj %>%
    set_attribute("aDAPT.Rx",0) %>%
    set_attribute("aDAPT.SecondLine",
                  function() {
                    if(inputs[["Clopidogrel"]]$vDAPT.SecondLine == "Ticagrelor")       {return(2)} else
                    if(inputs[["Clopidogrel"]]$vDAPT.SecondLine == "Prasugrel")        {return(3)} 
                    # Something went very wrong
                    stop("Invalid Logic in assigning DAPT medication")
                  }) %>%
    branch(
      function(attrs) {
        # Under the prospective genotyping scenario, the genotyped patients are switched with some probability.  
        if(inputs[["Global"]]$Scenario == "PGx-Prospective" & attrs[['aGenotyped']]==1 & attrs[['aCYP2C19']] == 1 & attrs[['aDAPT.Rx.Hx']]==0 ) {
          return(sample(c(1,attrs[['aDAPT.SecondLine']]),1,prob=c(1-inputs[["Clopidogrel"]]$vProbabilityDAPTSwitch,inputs[["Clopidogrel"]]$vProbabilityDAPTSwitch)))
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
      function(attrs) ifelse(attrs[['aNumDAPT']]<inputs[["Clopidogrel"]]$vMaxDAPT,1,2),
      merge = c(TRUE,TRUE),
      create_trajectory() %>%  
        mark("DAPT Initiated")  %>% 
        set_attribute("aRRDAPT",inputs[["Clopidogrel"]]$vRRRepeat.DAPT)  %>% 
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
        set_attribute("aST",function(attrs) now(env) + time_to_ST(attrs))
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
     return( inputs[["Clopidogrel"]]$vDAPT.Tx.Duration )
   } else
     return(end.of.model +1)
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

time_to_ST = function(attrs) 
{
  if (attrs[["aOnDAPT"]]!=1) return(end.of.model+1) else
  {
    # Relative Risk
    rr = attrs[["aRR.DAPT.ST"]]
    if (attrs[['aDAPT.Rx']]==2) rr = inputs[["Clopidogrel"]]$vRR.ST.Ticagrelor 
    if (attrs[['aDAPT.Rx']]==3) rr = inputs[["Clopidogrel"]]$vRR.ST.Prasugrel
    if (attrs[['aDAPT.Rx']]==4) rr = inputs[["Clopidogrel"]]$vRR.ST.Aspirin
      
    # Baseline Risk
    rates = c(inputs[["Clopidogrel"]]$vRiskST30,inputs[["Clopidogrel"]]$vRiskST365,inputs[["Clopidogrel"]]$vRiskSTgt365)
    days = c(30,365-30,365*4-365)
    
    # Convert To Probability 
    rates2 = (- (log ( 1 - rates)) / days)*rr
    
    timeST = rpexp(1, rate=c(rates2,epsilon), t=c(0,days))
    return(timeST)
    
  }
}

ST_event = function(traj) 
{
  traj %>%
    create_trajectory()  %>% mark("Stent Thrombosis") 
}




