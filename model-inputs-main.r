####
## 
# Define Simluation Inputs
##
####


## Global Run Parameters

epsilon = 0.000000001
end.of.model = 36500

inputs      <- list()
inputs[["Global"]]$Scenario = "PGx-Prospective"

## General Patient Attributes
inputs[["Attr"]]$vPctFemale = 0.5
inputs[["Attr"]]$vAge = 40


####
## 
# Clopidogrel Sub-Model
##
####
inputs[["Clopidogrel"]]$vDAPT.SecondLine <- "Ticagrelor"

# Prognostic Model
inputs[["Clopidogrel"]]$vSensitivityPrDAPT = 0.74
inputs[["Clopidogrel"]]$vSpecificityPrDAPT = 0.61

# Population-Level Allele Frequency Distribution
inputs[["Clopidogrel"]]$vCYP2C19.Poor = 0.21
inputs[["Clopidogrel"]]$vCYP2C19.Rapid = 0.33
inputs[["Clopidogrel"]]$vCYP2C19.Unknown = 0.07
inputs[["Clopidogrel"]]$vCYP2C19.Probs = c(inputs[["Clopidogrel"]]$vCYP2C19.Poor,
                                           inputs[["Clopidogrel"]]$vCYP2C19.Rapid,
                                           inputs[["Clopidogrel"]]$vCYP2C19.Unknown,
                                           1-inputs[["Clopidogrel"]]$vCYP2C19.Poor-inputs[["Clopidogrel"]]$vCYP2C19.Rapid-inputs[["Clopidogrel"]]$vCYP2C19.Unknown )

# Indication Paramters (Weibull) source: VUMC data -- files is ./reference/WCS_KM_Distribution_Generation.pdf
inputs[["Clopidogrel"]]$vDAPTShape = 0.59  
inputs[["Clopidogrel"]]$vDAPTScale = 60475.53

# This parameter governs whether repeat DAPT therapy is more or less likely after having one.
inputs[["Clopidogrel"]]$vRRRepeat.DAPT = epsilon


# This paramter governs the maximum number of DAPT therapies an individual can have.  The relative risk of DAPT is 
# set to epsilon (i.e., never re-occurs) once the patient has hit this maximum.
inputs[["Clopidogrel"]]$vMaxDAPT = 4

inputs[["Clopidogrel"]]$vDAPT.Tx.Duration = 365

inputs[["Clopidogrel"]]$vProbabilityDAPTSwitch = 0.55 # Source: VUMC PREDICT DATA


# Event Rates and Relative Risks
inputs[["Clopidogrel"]]$vRiskST30 = 0.0150
inputs[["Clopidogrel"]]$vRiskST365 = 0.0060
inputs[["Clopidogrel"]]$vRiskSTgt365 = 0.0022
inputs[["Clopidogrel"]]$vRR.ST.Ticagrelor = 0.75
inputs[["Clopidogrel"]]$vRR.ST.Prasugrel = 0.48
inputs[["Clopidogrel"]]$vRR.ST.Aspirin = 1.79
inputs[["Clopidogrel"]]$vSt.Case.Fatality = 0.20
inputs[["Clopidogrel"]]$vPrCABG.ST = 0.25







