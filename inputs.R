####
## 
# Define Simluation Inputs
##
####

# Function to Convert Nominal to Real Dollars

library(quantmod)

# Get CPI-U from FRED
getSymbols("CPIAUCSL", src='FRED')
avg.cpi <- apply.yearly(CPIAUCSL, mean)

realdol = function(nominal,year=2016,base=2012)
{
  cf <- avg.cpi/as.numeric(avg.cpi[paste(base)])
  return(as.numeric(nominal*cf[paste(year)]))
}


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

    vProbabilityDAPTSwitch = 1, #0.55, # Source: VUMC PREDICT DATA

    # Stent Thrombosis: Event Rates and Relative Risks
    
    # Relative Risk of ST for patients with loss of function allele who are treated with 
    # Clopidogrel.
    vRR.ST.LOF = 1.75, #(1.50-2.03) High Discrimination Scenario =  2.81 (1.81-4.37)
    
    # The Stent Thrombosis Risks are drawn from a piecewise exponential with the following
    # durations and rates. 
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

    
    vRR.MI.LOF = 1.48, #(1.05-2.07) High Discrimination Scenario = 1.45 (1.09-1.92)
    vRR.Mort.LOF = 1.28, #(0.95-1.73)
    vRR.Bleed.LOF = 0.84, # (0.75-1.00)
    vRR.Thrombotic.GOF = 0.75, # (0.66-1.00)
    vRR.Bleed.GOF = 1.26  # (1.00-1.50)
)

simvastatin <- list(
    vPREDICTsens = 0.74,    
    vPREDICTspec = 0.61,
    
    # Weibull for statin prescription
    vScale = 80722.66,
    vShape = 0.52,
    
    #
    vMedMetabolizer  = 0.249,   # Prevalence of medium metabolizers
    vPoorMetabolizer = 0.021,   # Prevalence of poor metabolizers
    
    vProbSimvastatinAlt = 1,  # Prob. of Alt | Variant
    vProbSimStopMild = 0.23,  # Prob. of Stop | Mild Myo
    vProbSimStopMod  = 0.23,  # Prob. of Stop | Mod Myo
    vProbSimStopSev  = 1.00,  # Prob. of Stop | Sev Myo
 
    # 5-year Mild Myopathy Risks
    vMildMyoBaseNoVar=1e-7, # No Drug Risk of mild myopathy
    vMildMyoSimNoVar=0.05,  # Simvastatin Mild Myopathy Baseline Risk
    vMildMyoSimMedVar=1,    # Rel Risk|Medium metabolizer
    vMildMyoSimPoorVar=1,   # Rel Risk|Poor metabolizer
    vMildMyoAltNoVar=0.05,  # Alternate Drug Mild Myopathy Baseline Risk
    vMildMyoAltMedVar=1,    # Rel Risk|Medium metabolizer
    vMildMyoAltPoorVar=1,    # Rel Risk|Poor metabolizer

    # 5-year Moderate Myopathy Risks
    vModMyoBaseNoVar=1e-10, # No Drug Risk of mild myopathy
    vModMyoSimNoVar=0.00011,  # Simvastatin Mild Myopathy Baseline Risk
    vModMyoSimMedVar=2.55,    # Rel Risk|Medium metabolizer
    vModMyoSimPoorVar=9.56,   # Rel Risk|Poor metabolizer
    vModMyoAltNoVar=0.00011,  # Alternate Drug Mild Myopathy Baseline Risk
    vModMyoAltMedVar=1.08,    # Rel Risk|Medium metabolizer
    vModMyoAltPoorVar=4.05,    # Rel Risk|Poor metabolizer

    # 5-year Severe Myopathy Risks
    vSevMyoBaseNoVar=1e-16,   # No Drug Risk of mild myopathy
    vSevMyoSimNoVar=0.000034, # Simvastatin Mild Myopathy Baseline Risk
    vSevMyoSimMedVar=2.55,    # Rel Risk|Medium metabolizer
    vSevMyoSimPoorVar=9.56,   # Rel Risk|Poor metabolizer
    vSevMyoAltNoVar=0.000034, # Alternate Drug Mild Myopathy Baseline Risk
    vSevMyoAltMedVar=1.08,    # Rel Risk|Medium metabolizer
    vSevMyoAltPoorVar=4.05   # Rel Risk|Poor metabolizer

)

warfarin = list(
    yadda_yadda = TRUE # ... More here
)

inputs <- list(
  # Population Parameters
  vN           = 1000,   # Patients to simulate
  vNIter       = 10,      # Number of Iterations (parallel processing)
  vLowerAge    = 55,      # Lower age to simulate coming in (uniform distribution)
  vUpperAge    = 55,      # Upper age to simulate
  vHorizon     = 100,      # Length of simulation upon a patient entering
  vPctFemale   = 0.5,     # Percent Female
  
  # Strategies
  vPreemptive  = "None",  # Can be one of following: "None", "Panel", "PREDICT", or "Age >= 50"
  vReactive    = "None", # Can be one of following: "None", "Single", "Panel"

# Control Which Drugs Are Run in the Model 
  vDrugs       = list(vSimvastatin = FALSE, 
                      vWarfarin = FALSE,
                      vClopidogrel = TRUE),

# CURRENTLY PANEL IS FOR ALL DRUGS ???
  vPanel       = list(vSimvastatin = TRUE, 
                    vWarfarin=FALSE, 
                    vClopidogrel = TRUE),

  # Drug specific model parameters
  clopidogrel = clopidogrel,
  simvastatin = simvastatin,
  warfarin    = warfarin,
  
  # If these names match the event names from the simmer model, then computation can be generalized!
  # These must be DAILY costs
  costs = list(
    panel_test      =   realdol(250,year=2012),
    single_test     =   realdol(100,year=2012),
    mild_myopathy   =   realdol(129,year=2012),
    mod_myopathy    =  realdol(2255/30,year=2012), # Note this divided by duration
    sev_myopathy    = realdol(12811/30,year=2012),
    cvd             = realdol(20347/30,year=2012),
    simvastatin     =   realdol(147/365,year=2012),
    alt_simvastatin = realdol(173.1/365,year=2012),
    
    aspirin         = realdol(4/30,year=2011),
    clopidogrel     = realdol(30/30,year=2011),
    ticagrelor      = realdol(220/30,year=2011),
    prasugrel       = realdol(261/30,year=2011),
    timi_ext_maj_nonfatal = realdol(10120/14,2011),
    timi_int_maj_nonfatal = realdol(20740,2011),
    timi_min_nonfatal = realdol(79/2,2011),
    fatal_bleed     = realdol(17920,2011),
    st_fatal        = realdol(24540,2011),
    cabg_mi            = realdol(67720,2011),
    mi_med_manage   = realdol(17200,2011),
    mi_nonfatal     = realdol(27840,2011),
    revasc_cabg     = realdol(50560/14,2011),
    revasc_pci      = realdol(20670/7,2011)
    
    

  ),
  # Each listed duration will be corrected in the final data frame
  durations = list(
    mild_myopathy =  1,
    mod_myopathy  = 30,
    sev_myopathy  = 30,
    cvd           = 30,
    single_test   =  1,
   
    timi_ext_maj_nonfatal = 14,
    timi_int_maj_nonfatal = 1,
    timi_min_nonfatal = 2,
    revasc_cabg     = 14,
    revasc_pci      = 7 
    
  ),
  disutilities = list(
    mild_myopathy = 0.0100,
    mod_myopathy  = 0.0500,
    sev_myopathy  = 0.5300,
    cvd           = 0.2445,
    
    timi_ext_maj_nonfatal = .2,
    timi_int_maj_nonfatal = .61,
    timi_min_nonfatal = .2,
    fatal_bleed     = 1,
    st_fatal        = 1,
    cabg_mi         = .12,
    mi_med_manage   = .12,
    mi_nonfatal     = .12,
    revasc_cabg     = .5,
    revasc_pci      = .5 
  )
  
)   







