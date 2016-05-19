#################################################################################################################################
# Dual Antiplatelet Therapy Parameters 

# To Do: Seize and Release Drugs

# Indication Paramters (Weibull) source: VUMC data -- files is ./reference/WCS_KM_Distribution_Generation.pdf
inputs$vDAPTShape = 0.59  
inputs$vDAPTScale = 60475.53

# This parameter governs whether repeat DAPT therapy is more or less likely after having one.
inputs$vRRRepeat.DAPT = epsilon

# This paramter governs the maximum number of DAPT therapies an individual can have.  The relative risk of DAPT is 
# set to epsilon (i.e., never re-occurs) once the patient has hit this maximum.
inputs$vMaxDAPT = 4

inputs$vDAPT.Tx.Duration = 365

inputs$vProbabilityDAPTSwitch = 0.55 # Source: VUMC PREDICT DATA

#################################################################################################################################

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
  inputs$vDAPTScale * (-log(aRandUnif)*exp(-log(aLPEvent)))^(1/inputs$vDAPTShape)
}

# Sanity Check: Should be ~4%
# x = c()
# for (i in 1:10000) x[i] = days_till_dapt(1); prop.table(table(x<=365))


######
## Assign DAPT Medication

assign_DAPT_medication <- function(traj,inputs=list()) 
{
  traj %>%
    set_attribute("aDAPT.Rx",0) %>%
    set_attribute("aDAPT.SecondLine",
                  function() {
                    if(inputs$vDAPT.SecondLine == "Ticagrelor")       {return(2)} else
                    if(inputs$vDAPT.SecondLine == "Prasugrel")        {return(3)} 
                    # Something went very wrong
                    stop("Invalid Logic in assigning DAPT medication")
                  }) %>%
    branch(
      function(attrs) {
        # Under the prospective genotyping scenario, the genotyped patients are switched with some probability.  
        if(inputs$Scenario == "PGx-Prospective" & attrs[['aGenotyped']]==1 & attrs[['aCYP2C19']] == 1 & attrs[['aDAPT.Rx.Hx']]==0 ) {
          return(sample(c(1,attrs[['aDAPT.SecondLine']]),1,prob=c(1-inputs$vProbabilityDAPTSwitch,inputs$vProbabilityDAPTSwitch)))
        } else 
        if (attrs[['aDAPT.Rx.Hx']]!=0) {return(attrs[['aDAPT.Rx.Hx']])} 
        return(1)
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
      create_trajectory() %>% mark("Initiated Aspirin") %>% seize("Aspirin") %>% set_attribute("aAspirin",1)
     )
  }

dapt <- function(traj)
{
  traj %>%
    branch( 
      function(attrs) ifelse(attrs[['aNumDAPT']]<inputs$vMaxDAPT,1,2),
      merge = c(TRUE,TRUE),
      create_trajectory() %>%  
        mark("DAPT Initiated")  %>% 
        set_attribute("aRRDAPT",inputs$vRRRepeat.DAPT)  %>% 
        set_attribute("aNumDAPT",function(attrs) attrs[['aNumDAPT']]+1) %>%
        assign_DAPT_medication(inputs)
      ,
      create_trajectory() %>% set_attribute("aRRDAPT",epsilon)
    ) 
}

