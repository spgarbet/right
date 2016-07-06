####
## 
# Define Simluation Inputs
##
####

epsilon <- 0.000000000001



clopidogrel = list(
#    vPREDICTsens = 0.3,    
#    vPREDICTspec = 0.3,

    vDAPT.SecondLine = "Ticagrelor",

    # Prognostic Model
    vSensitivityPrDAPT = 0.74,
    vSpecificityPrDAPT = 0.61,

    # Population-Level Allele Frequency Distribution
    vCYP2C19.Poor    = 0.21, # (0.15-0.40)
    vCYP2C19.Rapid   = 0.33, # (0.10-.40)
    vCYP2C19.Unknown = 0.07, # (0.05-0.09)

    # Indication Paramters (Weibull) source: VUMC data -- files is ./reference/WCS_KM_Distribution_Generation.pdf
    vDAPTShape = 0.59,
    vDAPTScale = 60475.53,

    # This parameter governs whether repeat DAPT therapy is more or less likely after having one.
    vRRRepeat.DAPT = epsilon,

    # This paramter governs the maximum number of DAPT therapies an individual can have.  The relative risk of DAPT is 
    # set to epsilon (i.e., never re-occurs) once the patient has hit this maximum.
    vMaxDAPT = 4,

    vDAPT.Tx.Duration = 365, # (12mo-48mo)

    vProbabilityDAPTSwitch = 0.55, # Source: VUMC PREDICT DATA

    # Stent Thrombosis: Event Rates and Relative Risks
    vRiskST30    = 0.0150, # (0.010-0.020)
    vRiskST365   = 0.0060, # (0.003-0.009)
    vRiskSTgt365 = 0.0022, # (0.001-0.003)

    vRR.ST.Ticagrelor = 0.75, # (0.59-0.95)
    vRR.ST.Prasugrel  = 0.48, # (0.36-0.64)
    vRR.ST.Aspirin    = 1.29, # (1.12-1.48)
    vSt.Case.Fatality = 0.20, #(15-30)
    vPrCABG.ST = 0.10,  # WHAT IS SOURCE?  CAN'T FIND IN ANNALS PAPER...

    # Myocardial Infarction: Event Rates and Relative Risks
    vRiskMI = 0.035, #(0.013-0.097)
    vRR.MI.Ticagrelor =0.84, # (0.75-0.95)
    vRR.MI.Prasugrel = 0.76, # (0.67-0.85)
    vRR.MI.Aspirin = 1.29, # (1.12-1.48)
    vPrCABG.MI = 0.08, # (4-12)
    vPrPCI.MI = 0.55, # (45-65)

    # Revascularization
    vRiskRV365 = 0.10, # (0.05-0.15)
    vRiskRVgt365 = 0.03, # (0.02-0.04)
    vPrCABG.RV = .25, # (15-35)

    # Bleeding
    vRiskExtBleed = 0.0230, # (0.015-0.070)
    vRiskIntBleed = 0.0015, # (0.001-0.002)
    vRiskTIMIMinor = 0.0200, # (0.010-0.060)
    vRiskFatalBleed = 0.0015, # (0.001-0.003)
    vRiskCABGTIMImajor = 0.0350, # (0.013-0.097)
    vRR.ExtBleed.Ticagrelor = 1.30, # (1.05-1.61)
    vRR.ExtBleed.Prasugrel = 1.22, #(0.93-1.6)
    vRR.ExtBleed.Aspirin =  0.72, #(0.60-1.00)

    vRR.IntBleed.Ticagrelor = 1.15, # (.55-2.41)
    vRR.IntBleed.Prasugrel =  0.83, # (0.36-1.92)
    vRR.IntBleed.Aspirin =  0.71, # (0.23-2.23)

    vRR.TIMIMinor.Ticagrelor = 1.07, # ( .91 - 1.26)
    vRR.TIMIMinor.Prasugrel =  1.16, # (.91-1.49)
    vRR.TIMIMinor.Aspirin =  0.47, #(.39-.57)

    vRR.FatalBleed.Ticagrelor = 1.35, #( 0.62-0.95)
    vRR.FatalBleed.Prasugrel =  0.87, # (0.48-1.59)
    vRR.FatalBleed.Aspirin =  4.19, # (1.58-11.11)

    vRR.RiskCABGTIMImajor.Ticagrelor = 1.08, # (0.61-1.91)
    vRR.RiskCABGTIMImajor.Prasugrel =  1.08, # (0.85-1.36)
    vRR.RiskCABGTIMImajor.Aspirin =  4.73,# (1.90-11.82)

    vRR.ST.LOF = 1.75, #(1.50-2.03) High Discrimination Scenario =  2.81 (1.81-4.37)
    vRR.MI.LOF = 1.48, #(1.05-2.07) High Discrimination Scenario = 1.45 (1.09-1.92)
    vRR.Mort.LOF = 1.28, #(0.95-1.73)
    vRR.Bleed.LOF = 0.84, # (0.75-1.00)
    vRR.Thrombotic.GOF = 0.75, # (0.66-1.00)
    vRR.Bleed.GOF = 1.26  # (1.00-1.50)
)

simvastatin <- list(
    vPREDICTsens = 0.3,    
    vPREDICTspec = 0.3,
    vMedMetabolizer  = 0.249,   # Prevalence of medium metabolizers
    vPoorMetabolizer = 0.021,   # Prevalence of poor metabolizers
    
    vProbSimvastatinAlt = 1,  # Prob. of Alt | Variant
    vProbSimStopMild = 0.23,  # Prob. of Stop | Mild Myo
    vProbSimStopMod  = 0.23,  # Prob. of Stop | Mod Myo
    vProbSimStopSev  = 1.00,  # Prob. of Stop | Sev Myo
 
    # Mild Myopathy Risks
    vMildMyoBaseNoVar=1e-7, # No Drug Risk of mild myopathy
    vMildMyoSimNoVar=0.05,  # Simvastatin Mild Myopathy Baseline Risk
    vMildMyoSimMedVar=1,    # Rel Risk|Medium metabolizer
    vMildMyoSimPoorVar=1,   # Rel Risk|Poor metabolizer
    vMildMyoAltNoVar=0.05,  # Alternate Drug Mild Myopathy Baseline Risk
    vMildMyoAltMedVar=1,    # Rel Risk|Medium metabolizer
    vMildMyoAltPoorVar=1,    # Rel Risk|Poor metabolizer

    # Moderate Myopathy Risks
    vModMyoBaseNoVar=1e-10, # No Drug Risk of mild myopathy
    vModMyoSimNoVar=0.00011,  # Simvastatin Mild Myopathy Baseline Risk
    vModMyoSimMedVar=2.55,    # Rel Risk|Medium metabolizer
    vModMyoSimPoorVar=9.56,   # Rel Risk|Poor metabolizer
    vModMyoAltNoVar=0.00011,  # Alternate Drug Mild Myopathy Baseline Risk
    vModMyoAltMedVar=1.08,    # Rel Risk|Medium metabolizer
    vModMyoAltPoorVar=4.05,    # Rel Risk|Poor metabolizer

    # Moderate Myopathy Risks
    vSevMyoBaseNoVar=1e-16,   # No Drug Risk of mild myopathy
    vSevMyoSimNoVar=0.000034, # Simvastatin Mild Myopathy Baseline Risk
    vSevMyoSimMedVar=2.55,    # Rel Risk|Medium metabolizer
    vSevMyoSimPoorVar=9.56,   # Rel Risk|Poor metabolizer
    vSevMyoAltNoVar=0.000034, # Alternate Drug Mild Myopathy Baseline Risk
    vSevMyoAltMedVar=1.08,    # Rel Risk|Medium metabolizer
    vSevMyoAltPoorVar=4.05,   # Rel Risk|Poor metabolizer

    # This should be moved to costs section somehow....
    vCostSimvastatin=147,     # Yearly cost of simvastatin
    vCostAlternate=173.1      # Yearly cost of alternative
)

warfarin = list(
    yadda_yadda = TRUE # ... More here
)

inputs <- list(
  # Population Parameters
  vN           = 10000,   # Patients to simulate
  vLowerAge    = 40,      # Lower age to simulate coming in (uniform distribution)
  vUpperAge    = 40,      # Upper age to simulate
  vHorizon     = 10,      # Length of simulation upon a patient entering
  vPctFemale   = 0.5,     # Percent Female
  
  # Strategies
  vPreemptive  = "None",  # Can be one of following: "None", "Panel", "PREDICT", or "Age >= 50"
  vReactive    = "Panel", # Can be one of following: "None", "Single", "Panel"
# CURRENTLY PANEL IS FOR ALL DRUGS
#  vPanel       = list(vSimvastatin = TRUE, vWarfarin=FALSE, vClopidogrel = FALSE),

  # Drug specific model parameters
  clopidogrel = clopidogrel,
  simvastatin = simvastatin,
  warfarin    = warfarin,
  
  # If these names match the event names from the simmer model, then computation can be generalized!
  costs = list(
    panel_test    =   250,
    single_test   =   100,
    mild_myopathy =   129,
    mod_myopathy  =  2255,
    sev_myopathy  = 12811,
    cvd           = 20347
  ),
  # Each listed duration will be corrected in the final data frame
  durations = list(
    mild_myopathy =  1,
    mod_myopathy  = 30,
    sev_myopathy  = 30,
    cvd           = 30
  ),
  disutilities = list(
    mild_myopathy = 0.0100,
    mod_myopathy  = 0.0500,
    sev_myopathy  = 0.5300,
    cvd           = 0.2445
  )
  
)   

# This should be folded into the input list, but for now to get this working is left as a global
end_of_model <- inputs$vHorizon  * 365 







