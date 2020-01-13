clopidogrel = list(
    vPREDICTsens = 0.23,
    vPREDICTspec = 0.93,
    vProbabilityRead = 1.00, # probability of physician using test results
    vProbabilityReactive = 1.00, # Under reactive, probability of ordering test
    vDAPT.SecondLine = "Ticagrelor",

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
    vRR.ExtBleed.Ticagrelor = 1.30, # (1.05-1.61)
    vRR.ExtBleed.Prasugrel = 1.22, #(0.93-1.6)
    vRR.ExtBleed.Aspirin =  0.72, #(0.60-1.00)

    vRR.IntBleed.Ticagrelor = 1.15, # (.55-2.41)
    vRR.IntBleed.Prasugrel =  0.83, # (0.36-1.92)
    vRR.IntBleed.Aspirin =  0.71, # (0.23-2.23)

    vRR.TIMIMinor.Ticagrelor = 1.07, # ( .91 - 1.26)
    vRR.TIMIMinor.Prasugrel =  1.16, # (.91-1.49)
    vRR.TIMIMinor.Aspirin =  0.47, #(.39-.57)

    vRR.FatalBleed.Ticagrelor = 0.87, # (0.48-1.59)
    vRR.FatalBleed.Prasugrel =  4.19, # (1.58-11.11)
    vRR.FatalBleed.Aspirin =  1.35, #(0.62-0.95)
    
    vRiskCABGTIMImajor = 0.022, # (0.013-0.031) 
    vRR.RiskCABGTIMImajor.Ticagrelor = 1.08, # (0.85-1.36)
    vRR.RiskCABGTIMImajor.Prasugrel =  4.73,# (1.90-11.82)
    vRR.RiskCABGTIMImajor.Aspirin =  1.08, # (0.61-1.91)

    vRR.MI.LOF = 1.48, #(1.05-2.07) High Discrimination Scenario = 1.45 (1.09-1.92)
    vRR.Mort.LOF = 1.28, #(0.95-1.73) #Not sure how to use this one
    vRR.Bleed.LOF = 0.84, # (0.75-1.00)
    vRiskStroke = epsilon
)