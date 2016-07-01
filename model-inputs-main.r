####
## 
# Define Simluation Inputs
##
####


## Global Run Parameters

epsilon = 0.000000000001
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
inputs[["Clopidogrel"]]$vCYP2C19.Poor = 0.21 # (0.15-0.40)
inputs[["Clopidogrel"]]$vCYP2C19.Rapid = 0.33 # (0.10-.40)
inputs[["Clopidogrel"]]$vCYP2C19.Unknown = 0.07 # (0.05-0.09)
inputs[["Clopidogrel"]]$vCYP2C19.Probs = c(inputs[["Clopidogrel"]]$vCYP2C19.Poor,
                                           inputs[["Clopidogrel"]]$vCYP2C19.Rapid,
                                           inputs[["Clopidogrel"]]$vCYP2C19.Unknown,
                                           1-inputs[["Clopidogrel"]]$vCYP2C19.Poor-inputs[["Clopidogrel"]]$vCYP2C19.Rapid-inputs[["Clopidogrel"]]$vCYP2C19.Unknown )

# Indication Paramters (Weibull) source: VUMC data -- files is ./reference/WCS_KM_Distribution_Generation.pdf
inputs[["Clopidogrel"]]$vDAPTShape = 1#0.59  
inputs[["Clopidogrel"]]$vDAPTScale = 1#60475.53

# This parameter governs whether repeat DAPT therapy is more or less likely after having one.
inputs[["Clopidogrel"]]$vRRRepeat.DAPT = epsilon

# This paramter governs the maximum number of DAPT therapies an individual can have.  The relative risk of DAPT is 
# set to epsilon (i.e., never re-occurs) once the patient has hit this maximum.
inputs[["Clopidogrel"]]$vMaxDAPT = 4

inputs[["Clopidogrel"]]$vDAPT.Tx.Duration = 365 # (12mo-48mo)

inputs[["Clopidogrel"]]$vProbabilityDAPTSwitch = 0.55 # Source: VUMC PREDICT DATA

# Stent Thrombosis: Event Rates and Relative Risks
inputs[["Clopidogrel"]]$vRiskST30 = 0.0150  # (0.010-0.020)
inputs[["Clopidogrel"]]$vRiskST365 = 0.0060 # (0.003-0.009)
inputs[["Clopidogrel"]]$vRiskSTgt365 = 0.0022 # (0.001-0.003)

inputs[["Clopidogrel"]]$vRR.ST.Ticagrelor = 0.75 # (0.59-0.95)
inputs[["Clopidogrel"]]$vRR.ST.Prasugrel = 0.48 # (0.36-0.64)
inputs[["Clopidogrel"]]$vRR.ST.Aspirin = 1.29 # (1.12-1.48)
inputs[["Clopidogrel"]]$vSt.Case.Fatality = 0.20 #(15-30)
inputs[["Clopidogrel"]]$vPrCABG.ST = 0.10  # WHAT IS SOURCE?  CAN'T FIND IN ANNALS PAPER...

# Myocardial Infarction: Event Rates and Relative Risks
inputs[["Clopidogrel"]]$vRiskMI = 0.035 #(0.013-0.097)
inputs[["Clopidogrel"]]$vRR.MI.Ticagrelor =0.84 # (0.75-0.95)
inputs[["Clopidogrel"]]$vRR.MI.Prasugrel = 0.76 # (0.67-0.85)
inputs[["Clopidogrel"]]$vRR.MI.Aspirin = 1.29 # (1.12-1.48)
inputs[["Clopidogrel"]]$vPrCABG.MI = 0.08 # (4-12)
inputs[["Clopidogrel"]]$vPrPCI.MI = 0.55 # (45-65)

# Revascularization
inputs[["Clopidogrel"]]$vRiskRV365 = 0.10 # (0.05-0.15)
inputs[["Clopidogrel"]]$vRiskRVgt365 = 0.03 # (0.02-0.04)
inputs[["Clopidogrel"]]$vPrCABG.RV = .25 # (15-35)

# Bleeding
inputs[["Clopidogrel"]]$vRiskExtBleed = 0.0230 # (0.015-0.070)
inputs[["Clopidogrel"]]$vRiskIntBleed = 0.0015 # (0.001-0.002)
inputs[["Clopidogrel"]]$vRiskTIMIMinor = 0.0200 # (0.010-0.060)
inputs[["Clopidogrel"]]$vRiskFatalBleed = 0.0015 # (0.001-0.003)
inputs[["Clopidogrel"]]$vRiskCABGTIMImajor = 0.0350 # (0.013-0.097)
inputs[["Clopidogrel"]]$vRR.ExtBleed.Ticagrelor = 1.30 # (1.05-1.61)
inputs[["Clopidogrel"]]$vRR.ExtBleed.Prasugrel = 1.22 #(0.93-1.6)
inputs[["Clopidogrel"]]$vRR.ExtBleed.Aspirin =  0.72 #(0.60-1.00)

inputs[["Clopidogrel"]]$vRR.IntBleed.Ticagrelor = 1.15 # (.55-2.41)
inputs[["Clopidogrel"]]$vRR.IntBleed.Prasugrel =  0.83 # (0.36-1.92)
inputs[["Clopidogrel"]]$vRR.IntBleed.Aspirin =  0.71 # (0.23-2.23)

inputs[["Clopidogrel"]]$vRR.TIMIMinor.Ticagrelor = 1.07 # ( .91 - 1.26)
inputs[["Clopidogrel"]]$vRR.TIMIMinor.Prasugrel =  1.16 # (.91-1.49)
inputs[["Clopidogrel"]]$vRR.TIMIMinor.Aspirin =  0.47 #(.39-.57)

inputs[["Clopidogrel"]]$vRR.FatalBleed.Ticagrelor = 1.35 #( 0.62-0.95)
inputs[["Clopidogrel"]]$vRR.FatalBleed.Prasugrel =  0.87 # (0.48-1.59)
inputs[["Clopidogrel"]]$vRR.FatalBleed.Aspirin =  4.19 # (1.58-11.11)

inputs[["Clopidogrel"]]$vRR.RiskCABGTIMImajor.Ticagrelor = 1.08 # (0.61-1.91)
inputs[["Clopidogrel"]]$vRR.RiskCABGTIMImajor.Prasugrel =  1.08 # (0.85-1.36)
inputs[["Clopidogrel"]]$vRR.RiskCABGTIMImajor.Aspirin =  4.73# (1.90-11.82)

inputs[["Clopidogrel"]]$vRR.ST.LOF = 1.75 #(1.50-2.03) High Discrimination Scenario =  2.81 (1.81-4.37)
inputs[["Clopidogrel"]]$vRR.MI.LOF = 1.48 #(1.05-2.07) High Discrimination Scenario = 1.45 (1.09-1.92)
inputs[["Clopidogrel"]]$vRR.Mort.LOF = 1.28 #(0.95-1.73)
inputs[["Clopidogrel"]]$vRR.Bleed.LOF = 0.84 # (0.75-1.00)
inputs[["Clopidogrel"]]$vRR.Thrombotic.GOF = 0.75 # (0.66-1.00)
inputs[["Clopidogrel"]]$vRR.Bleed.GOF = 1.26 # (1.00-1.50)








