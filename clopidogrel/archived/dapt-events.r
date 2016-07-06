#################################################################################################################################
# Dual Antiplatelet Therapy Event Parameters 
#
# Stent Thrombosis (Source: Annals Paper)
  # Piecewise Constant
  library(msm)
  st.rate = c(inputs[["Clopidogrel"]]$vRiskST30,
              inputs[["Clopidogrel"]]$vRiskST365,
              inputs[["Clopidogrel"]]$vRiskSTgt365)  # These are the rates
  st.dur = c(30,365-30,1460-365) # These are the time durations for the rates
  st.t = c(0,31,366) # Thest are the times at whice the rates change
  
# Sanity Check
   # rr = 1
    #foo = rpexp(n=100000,rate=-log(1-st.rate)*rr/st.dur,t=st.t); prop.table(table(foo<365))

# Revascularization (Source: Annals Paper)
   # Piecewise Constant
   revasc.rate = c(inputs[["Clopidogrel"]]$vRiskRV365,inputs[["Clopidogrel"]]$vRiskRVgt365)
   revasc.t = c(0,366)
   revasc.dur = c(365,1460-365)
   
# Stent Thrombosis Rate Ratios (Source: Annals Paper)
   #Stent thrombosis, relative to clopidogrel and aspirin
    #Aspirin monotherapy 1.29 
    #Ticagrelor and aspirin 0.75 
    #Prasugrel and aspirin 0.48
    #    Genetic testing
    #    Clinical events among carriers of loss-of-function alleles treated with
    #    clopidogrel, relative to noncarriers treated with clopidogrel Stent thrombosis
    #    Low-discrimination scenario 1.75
   #     High-discrimination scenario 2.81
   
   vRR.ST  =  c(1,
                inputs[["Clopidogrel"]]$vRR.ST.Ticagrelor,
                inputs[["Clopidogrel"]]$vRR.ST.Prasugrel)
   vRR.ST.LOF = c(inputs[["Clopidogrel"]]$vRR.ST.LOF,
                  inputs[["Clopidogrel"]]$vRR.ST.Ticagrelor,
                  inputs[["Clopidogrel"]]$vRR.ST.Prasugrel)
#
#################################################################################################################################

#### 
## Stent Thrombosis

days_till_st <- function(attrs) 
{
  drug = attrs[["aDAPT.Rx"]]
  phenotype = attrs[["aCYP2C19"]]
  
  rr <- if (phenotype==1) 
  {
    vRR.ST.LOF[drug] 
  } else 
  {
    vRR.ST[drug]
  }
 
  time.to.st = rpexp(n=1,rate=-log(1-st.rate)*rr/st.dur,t=st.t)
  if (time.to.st > vDAPT.Tx.Duration) {return(end.of.model+1)}
  return(time.to.st)
}

####
## Revasc
days_till_revasc <- function(attrs) 
{
  rr = 1
  time.to.revasc = rpexp(n=1,rate=-log(1-revasc.rate)*rr/revasc.dur,t=revasc.t)
  if (time.to.revasc > vDAPT.Tx.Duration) {return(end.of.model+1)}
}   
  

