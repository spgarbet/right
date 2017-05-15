#####NOTES
##strategy choice, not listed 
#vDAPT.SecondLine

##assumption to prevent recurrence
#vRRRepeat.DAPT
#vMaxDAPT

##need to confirm
#vProbabilityRead
#vProbabilityReactive
#vDAPTShape
#vDAPTScale
#vProbabilityDAPTSwitch
#vPrCABG.ST 

##disutility and durations, Kazi paper only gives range for one of them (temp disutility events)
##uncertainty around duration (fixed disutilities) 
#bleed_ext_maj_nonfatal
#bleed_min_nonfatal
#revasc_cabg
#revasc_pci
#cabg_bleed

parameters <- list(
  clopidogrel = list(
    vPREDICTsens = list(
      base_value = 0.23,
      type = "beta",
      min = 0.23,
      max = 0.23  
    ),
    vPREDICTspec = list(
      base_value = 0.93,
      type = "beta",
      min = 0.93,
      max = 0.93  
    ),
    vProbabilityRead = list(
      base_value = 1,
      type = "beta",
      min = 1,
      max = 1  
    ),
    vProbabilityReactive = list(
      base_value = 1,
      type = "beta",
      min = 1,
      max = 1  
    ),
    vCYP2C19.Poor = list(
      base_value = 0.21,
      type = "Dirichlet multinomial",
      min = 0.15,
      max = 0.4  
    ),
    vCYP2C19.Rapid = list(
      base_value = 0.33,
      type = "Dirichlet multinomial",
      min = 0.1,
      max = 0.4  
    ),
    vCYP2C19.Unknown = list(
      base_value = 0.07,
      type = "Dirichlet multinomial",
      min = 0.05,
      max = 0.09  
    ),
    vDAPTShape = list(
      base_value = 0.59,
      type = "uniform",
      min = 0.59,
      max = 0.59  
    ),
    vDAPTScale = list(
      base_value = 60475.53,
      type = "uniform",
      min = 60475.53,
      max = 60475.53  
    ),
    #fixed, assumption
    vRRRepeat.DAPT = list(
      base_value = 1.00E-12,
      type = "uniform",
      min = 1.00E-12,
      max = 1.00E-12  
    ),
    #fixed, assumption
    vMaxDAPT = list(
      base_value = 4,
      type = "uniform",
      min = 4,
      max = 4  
    ),
    vDAPT.Tx.Duration = list(
      base_value = 365,
      type = "lognormal",
      min = 365,
      max = 1460  
    ),
    vProbabilityDAPTSwitch = list(
      base_value = 0.55,
      type = "beta",
      min = 0.55,
      max = 0.55  
    ),
    vRR.ST.LOF = list(
      base_value = 1.75,
      type = "lognormal",
      min = 1.5,
      max = 2.03  
    ),
    vRiskST30 = list(
      base_value = 0.015,
      type = "lognormal",
      min = 0.01,
      max = 0.02  
    ),
    vRiskST365 = list(
      base_value = 0.006,
      type = "lognormal",
      min = 0.003,
      max = 0.009  
    ),
    vRiskSTgt365 = list(
      base_value = 0.0022,
      type = "lognormal",
      min = 0.001,
      max = 0.003  
    ),
    vRR.ST.Ticagrelor = list(
      base_value = 0.75,
      type = "lognormal",
      min = 0.59,
      max = 0.95  
    ),
    vRR.ST.Prasugrel = list(
      base_value = 0.48,
      type = "lognormal",
      min = 0.36,
      max = 0.64  
    ),
    vRR.ST.Aspirin = list(
      base_value = 1.29,
      type = "lognormal",
      min = 1.12,
      max = 1.48  
    ),
    vSt.Case.Fatality = list(
      base_value = 0.2,
      type = "beta",
      min = 0.15,
      max = 0.3  
    ),
    vPrCABG.ST = list(
      base_value = 0.1,
      type = "beta",
      min = 0.1,
      max = 0.1  
    ),
    vRiskMI = list(
      base_value = 0.035,
      type = "lognormal",
      min = 0.013,
      max = 0.097  
    ),
    vRR.MI.Ticagrelor = list(
      base_value = 0.84,
      type = "lognormal",
      min = 0.75,
      max = 0.95  
    ),
    vRR.MI.Prasugrel = list(
      base_value = 0.76,
      type = "lognormal",
      min = 0.67,
      max = 0.85  
    ),
    vRR.MI.Aspirin = list(
      base_value = 1.29,
      type = "lognormal",
      min = 1.12,
      max = 1.48  
    ),
    vPrCABG.MI = list(
      base_value = 0.08,
      type = "Dirichlet multinomial",
      min = 0.04,
      max = 0.12  
    ),
    vPrPCI.MI = list(
      base_value = 0.55,
      type = "Dirichlet multinomial",
      min = 0.45,
      max = 0.65  
    ),
    vRiskRV365 = list(
      base_value = 0.1,
      type = "lognormal",
      min = 0.05,
      max = 0.15  
    ),
    vRiskRVgt365 = list(
      base_value = 0.03,
      type = "lognormal",
      min = 0.02,
      max = 0.04  
    ),
    vPrCABG.RV = list(
      base_value = 0.25,
      type = "beta",
      min = 0.15,
      max = 0.35  
    ),
    vRiskExtBleed = list(
      base_value = 0.023,
      type = "lognormal",
      min = 0.05,
      max = 0.15  
    ),
    vRiskIntBleed = list(
      base_value = 0.0015,
      type = "lognormal",
      min = 0.001,
      max = 0.002  
    ),
    vRiskTIMIMinor = list(
      base_value = 0.02,
      type = "lognormal",
      min = 0.01,
      max = 0.06  
    ),
    vRiskFatalBleed = list(
      base_value = 0.0015,
      type = "lognormal",
      min = 0.001,
      max = 0.003  
    ),
    vRR.ExtBleed.Ticagrelor = list(
      base_value = 1.3,
      type = "lognormal",
      min = 1.05,
      max = 1.61  
    ),
    vRR.ExtBleed.Prasugrel = list(
      base_value = 1.22,
      type = "lognormal",
      min = 0.93,
      max = 1.6  
    ),
    vRR.ExtBleed.Aspirin = list(
      base_value = 0.72,
      type = "lognormal",
      min = 0.6,
      max = 1  
    ),
    vRR.IntBleed.Ticagrelor = list(
      base_value = 1.15,
      type = "lognormal",
      min = 0.55,
      max = 2.41  
    ),
    vRR.IntBleed.Prasugrel = list(
      base_value = 0.83,
      type = "lognormal",
      min = 0.36,
      max = 1.92  
    ),
    vRR.IntBleed.Aspirin = list(
      base_value = 0.71,
      type = "lognormal",
      min = 0.23,
      max = 2.23  
    ),
    vRR.TIMIMinor.Ticagrelor = list(
      base_value = 1.07,
      type = "lognormal",
      min = 0.91,
      max = 1.26  
    ),
    vRR.TIMIMinor.Prasugrel = list(
      base_value = 1.16,
      type = "lognormal",
      min = 0.91,
      max = 1.49  
    ),
    vRR.TIMIMinor.Aspirin = list(
      base_value = 0.47,
      type = "lognormal",
      min = 0.39,
      max = 0.57  
    ),
    vRR.FatalBleed.Ticagrelor = list(
      base_value = 1.35,
      type = "lognormal",
      min = 0.62,
      max = 2.95  
    ),
    vRR.FatalBleed.Prasugrel = list(
      base_value = 0.87,
      type = "lognormal",
      min = 0.48,
      max = 1.59  
    ),
    vRR.FatalBleed.Aspirin = list(
      base_value = 4.19,
      type = "lognormal",
      min = 1.58,
      max = 11.11  
    ),    
    vRiskCABGTIMImajor = list(
      base_value = 0.022,
      type = "lognormal",
      min = 0.013,
      max = 0.031  
    ),
    vRR.RiskCABGTIMImajor.Ticagrelor = list(
      base_value = 1.08,
      type = "lognormal",
      min = 0.85,
      max = 1.36  
    ),
    vRR.RiskCABGTIMImajor.Prasugrel = list(
      base_value = 4.73,
      type = "lognormal",
      min = 1.9,
      max = 11.82  
    ),
    vRR.RiskCABGTIMImajor.Aspirin = list(
      base_value = 1.08,
      type = "lognormal",
      min = 0.61,
      max = 1.91  
    ),
    vRR.MI.LOF = list(
      base_value = 1.48,
      type = "lognormal",
      min = 1.05,
      max = 2.07  
    ),
    vRR.Bleed.LOF = list(
      base_value = 0.84,
      type = "lognormal",
      min = 0.75,
      max = 1  
    ),
  
  disutilities=list(
    bleed_int_maj_nonfatal = list(
      base_value = 0.61,
      type = "beta",
      min = 0.4,
      max = 0.8  
    ),
    st_pci = list(
      base_value = 0.12,
      type = "beta",
      min = 0.07,
      max = 0.16  
    ),
    st_cabg = list(
      base_value = 0.12,
      type = "beta",
      min = 0.07,
      max = 0.16  
    ),
    mi_cabg = list(
      base_value = 0.12,
      type = "beta",
      min = 0.07,
      max = 0.16  
    ),
    mi_med_manage = list(
      base_value = 0.12,
      type = "beta",
      min = 0.07,
      max = 0.16  
    ),
    mi_pci = list(
      base_value = 0.12,
      type = "beta",
      min = 0.07,
      max = 0.16  
    ),
  #fixed disutilities:
    #fixed
    bleed_ext_maj_nonfatal = list(
      base_value = 0.2,
      type = "beta",
      min = 0.2,
      max = 0.2  
    ),
    #fixed
    bleed_min_nonfatal = list(
      base_value = 0.2,
      type = "beta",
      min = 0.2,
      max = 0.2  
    ),
    #fixed
    revasc_cabg = list(
      base_value = 0.5,
      type = "beta",
      min = 0.5,
      max = 0.5  
    ),
    #fixed
    revasc_pci = list(
      base_value = 0.5,
      type = "beta",
      min = 0.5,
      max = 0.5  
    ),
    #fixed
    cabg_bleed = list(
      base_value = 0.5,
      type = "beta",
      min = 0.5,
      max = 0.5  
    ),
    #fixed
    bleed_fatal = list(
      base_value = 1,
      type = "beta",
      min = 1,
      max = 1  
    ),
    #fixed
    st_fatal = list(
      base_value = 1,
      type = "beta",
      min = 1,
      max = 1  
    )
),
  
  durations = list(
    bleed_ext_maj_nonfatal = list(
      base_value = 14,
      type = "lognormal",
      min = 7,
      max = 21  
    ),
    bleed_min_nonfatal = list(
      base_value = 2,
      type = "lognormal",
      min = 0,
      max = 7  
    ),
    revasc_cabg = list(
      base_value = 14,
      type = "lognormal",
      min = 7,
      max = 21  
    ),
    revasc_pci = list(
      base_value = 7,
      type = "lognormal",
      min = 3,
      max = 14  
    ),
    cabg_bleed = list(
      base_value = 7,
      type = "lognormal",
      min = 3,
      max = 14  
    )),

  costs = list(
    aspirin = list(
      base_value = 4,
      type = "lognormal",
      min = 2,
      max = 10  
    ),
    clopidogrel = list(
      base_value = 30,
      type = "lognormal",
      min = 4,
      max = 200  
    ),
    ticagrelor = list(
      base_value = 220,
      type = "lognormal",
      min = 150,
      max = 300  
    ),
    prasugrel = list(
      base_value = 261,
      type = "lognormal",
      min = 150,
      max = 300  
    ),
    bleed_ext_maj_nonfatal = list(
      base_value = 10120,
      type = "lognormal",
      min = 5060,
      max = 20240  
    ),
    bleed_int_maj_nonfatal = list(
      base_value = 20740,
      type = "lognormal",
      min = 10370,
      max = 41480  
    ),
    bleed_min_nonfatal = list(
      base_value = 79,
      type = "lognormal",
      min = 40,
      max = 158  
    ),
    bleed_fatal = list(
      base_value = 17920,
      type = "lognormal",
      min = 8950,
      max = 35840  
    ),
    st_fatal = list(
      base_value = 24540,
      type = "lognormal",
      min = 12270,
      max = 49080  
    ),
    st_pci = list(
      base_value = 27840,
      type = "lognormal",
      min = 13920,
      max = 55680  
    ),
    st_cabg = list(
      base_value = 67720,
      type = "lognormal",
      min = 33860,
      max = 135440  
    ),
    mi_cabg = list(
      base_value = 67720,
      type = "lognormal",
      min = 33860,
      max = 135440  
    ),
    mi_med_manage = list(
      base_value = 17200,
      type = "lognormal",
      min = 8600,
      max = 34400  
    ),
    mi_pci = list(
      base_value = 27840,
      type = "lognormal",
      min = 13920,
      max = 55680  
    ),
    revasc_cabg = list(
      base_value = 50560,
      type = "lognormal",
      min = 10340,
      max = 101120  
    ),
    revasc_pci = list(
      base_value = 20670,
      type = "lognormal",
      min = 10340,
      max = 41340  
    ),
    cabg_bleed = list(
      base_value = 35570,
      type = "lognormal",
      min = 17790,
      max = 71140  
    ) 
    )
  )
)
  
