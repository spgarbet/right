library(shiny)

source("run.R")
source("costs.R")

shinyServer(function(input, output) {

  #reactive UI panels for drug-specific parameters
  output$uim <- renderUI({
    if(length(input$wDrug)>0) {
      navlistPanel(
        
        tabPanel(
          "Clopidogrel",
          conditionalPanel(
            condition = "input.wDrug.includes('Clopidogrel')",
            tabsetPanel(
              tabPanel("Strategy",
                       br(),
                       selectInput("vDAPT.SecondLine", "DAPT: Alternative Drug",
                                   c("Ticagrelor", "Prasugrel"), "None", FALSE)),
              tabPanel(
                "Population",
                br(),
                h4("Phenotypic Prevalence:"),
                br(),
                sliderInput("vCYP2C19.Poor",  "Poor or Intermediate Metabolizer Prevalence",  value=0.21, min=0.15, max=1, step=0.01),
                sliderInput("vCYP2C19.Rapid", "Rapid Metabolizer Prevalence", value=0.33, min=0.1, max=1, step=0.01)
                #sliderInput("vCYP2C19.Unknown", "Unknown %", value=0.07, min=0.05, max=0.09, step=0.01)
                #sliderInput vs parameter/numeric input: need to limit bounds?, should not sum up over 1????
              ),
              tabPanel(
                "Risks",
                br(),
                h4("Stent Thrombosis:"),
                numericInput("vRiskST30",  "ST 30-day Baseline Risk (0.010-0.020)",  value=0.0150, min=0.010, max=0.020, step=0.005),
                numericInput("vRiskST365",  "ST 1-year Baseline Risk (0.003-0.009)",  value=0.0060, min=0.003, max=0.009, step=0.001),
                numericInput("vRiskSTgt365",  "ST post 1-year Baseline Risk (0.001-0.003)",  value=0.0022, min=0.001, max=0.003, step=0.0005),
                numericInput("vRR.ST.LOF",  "ST RR for Clopidogrel users with LOF (1.50-2.03)",  value=1.75, min=1.50, max=2.03, step=0.1),
                numericInput("vRR.ST.Ticagrelor", "ST RR for Ticagrelor users (0.59-0.95)", value=0.75, min=0.59, max=0.95, step=0.005),
                numericInput("vRR.ST.Prasugrel",  "ST RR for Prasugrel users (0.36-0.64)",  value=0.48, min=0.36, max=0.64, step=0.05),
                numericInput("vRR.ST.Aspirin",  "ST RR for Aspirin users (1.12-1.48)",  value=1.29, min=1.12, max=1.48, step=0.02),
                numericInput("vSt.Case.Fatality", "ST Case Fatality (0.15-0.30)", value=0.20, min=0.15, max=0.3, step=0.05),
                numericInput("vPrCABG.ST",  "ST CABG Probability",  value=0.10, min=0.05, max=0.30, step=0.05),
                
                br(),
                h4("Myocardial Infarction:"),
                numericInput("vRiskMI",  "MI Baseline Risk (0.013-0.097)",  value=0.035, min=0.013, max=0.097, step=0.002),
                numericInput("vRR.MI.LOF",  "MI RR for Clopidogrel users with LOF (1.05-2.07)",  value=1.48, min=1.05, max=2.07, step=0.02),
                numericInput("vRR.MI.Ticagrelor", "MI RR for Ticagrelor users (0.75-0.95)", value=0.84, min=0.75, max=0.95, step=0.05),
                numericInput("vRR.MI.Prasugrel",  "MI RR for Prasugrel users (0.67-0.85)",  value=0.76, min=0.67, max=0.85, step=0.02),
                numericInput("vRR.MI.Aspirin",  "MI RR for Aspirin users (1.12-1.48)",  value=1.29, min=1.12, max=1.48, step=0.02),
                numericInput("vPrCABG.MI", "MI CABG Probability (0.04-0.12)", value=0.08, min=0.04, max=0.12, step=0.04),
                numericInput("vPrPCI.MI",  "MI PCI Probability (0.45-0.65)",  value=0.55, min=0.45, max=0.65, step=0.05),
                
                br(),
                h4("Revascularization:"),
                numericInput("vRiskRV365",  "RV 1-year Baseline Risk (0.05-0.15)",  value=0.10, min=0.05, max=0.15, step=0.05),
                numericInput("vRiskRVgt365",  "RV post 1-year Baseline Risk (0.02-0.04)",  value=0.03, min=0.02, max=0.04, step=0.005),
                numericInput("vPrCABG.RV",  "RV CABG Probability  (0.15-0.35)",  value=0.25, min=0.15, max=0.35, step=0.05),
                
                br(),
                h4("Bleeding Events:"),
                numericInput("vRR.Bleed.LOF",  "Bleeding RR for Clopidogrel users with LOF (0.75-1.00)",  value=0.84, min=0.75, max=1, step=0.05),
                h5("Extracranial Bleeding:"),
                numericInput("vRiskExtBleed",  "Bleed Ext Maj Baseline Risk (0.015-0.070)",  value=0.0230, min=0.015, max=0.070, step=0.005),
                numericInput("vRR.ExtBleed.Ticagrelor", "Bleed Ext Maj RR for Ticagrelor users (1.05-1.61)", value=1.30, min=1.05, max=1.61, step=0.05),
                numericInput("vRR.ExtBleed.Prasugrel",  "Bleed Ext Maj RR for Prasugrel users (0.93-1.6)",  value=0.93, min=1.6, max=0.85, step=0.02),
                numericInput("vRR.ExtBleed.Aspirin",  "Bleed Ext Maj RR for Aspirin users (0.60-1.00)",  value=0.72, min=0.60, max=1.00, step=0.02),
                h5("Intracranial Bleeding:"),
                numericInput("vRiskIntBleed",  "Bleed Int Maj Baseline Risk (0.001-0.002)",  value=0.0015, min=0.001, max=0.002, step=0.0005),
                numericInput("vRR.IntBleed.Ticagrelor", "Bleed Int Maj RR for Ticagrelor users (0.55-2.41)", value=1.15, min=0.55, max=2.41, step=0.5),
                numericInput("vRR.IntBleed.Prasugrel",  "Bleed Int Maj RR for Prasugrel users (0.36-1.92)",  value=0.83, min=0.36, max=1.92, step=0.4),
                numericInput("vRR.IntBleed.Aspirin",  "Bleed Int Maj RR for Aspirin users (0.23-2.23)",  value=0.71, min=0.23, max=2.23, step=0.04),
                h5("TIMI Minor Bleeding:"),
                numericInput("vRiskTIMIMinor",  "TIMI Min Baseline Risk (0.010-0.060)",  value=0.0200, min=0.010, max=0.060, step=0.01),
                numericInput("vRR.TIMIMinor.Ticagrelor", "TIMI Min RR for Ticagrelor users (0.91-1.26)", value=1.07, min=0.91, max=1.26, step=0.02),
                numericInput("vRR.TIMIMinor.Prasugrel",  "TIMI Min RR for Prasugrel users (0.91-1.49)",  value=1.16, min=0.91, max=1.49, step=0.02),
                numericInput("vRR.TIMIMinor.Aspirin",  "TIMI Min RR for Aspirin users (0.39-0.57)",  value=0.47, min=0.39, max=0.57, step=0.02),
                h5("Fatal Bleeding:"),
                numericInput("vRiskFatalBleed",  "Bleed Ext Maj Baseline Risk (0.001-0.003)",  value=0.0015, min=0.001, max=0.003, step=0.0005),
                numericInput("vRR.FatalBleed.Ticagrelor", "Bleed Ext Maj RR for Ticagrelor users (0.48-1.59)", value=0.87, min=0.48, max=1.59, step=0.01),
                numericInput("vRR.FatalBleed.Prasugrel",  "Bleed Ext Maj RR for Prasugrel users (1.58-11.11)",  value=4.19, min=1.58, max=11.11, step=0.1),
                numericInput("vRR.FatalBleed.Aspirin",  "Bleed Ext Maj RR for Aspirin users (0.62-0.95)",  value=1.35, min=0.62, max=0.95, step=0.02),
                h5("CABG-related TIMI Major Bleeding:"),
                numericInput("vRiskCABGTIMImajor",  "CABG-related Bleed Baseline Risk (0.013-0.031)",  value=0.022, min=0.013, max=0.031, step=0.002),
                numericInput("vRR.RiskCABGTIMImajor.Ticagrelor", "CABG-related Bleed RR for Ticagrelor users (0.85-1.36)", value=1.08, min=0.85, max=1.36, step=0.01),
                numericInput("vRR.RiskCABGTIMImajor.Prasugrel",  "CABG-related Bleed RR for Prasugrel users (1.90-11.82)",  value=4.73, min=1.90, max=11.82, step=0.1),
                numericInput("vRR.RiskCABGTIMImajor.Aspirin",  "CABG-related Bleed RR for Aspirin users (0.61-1.91)",  value=1.08, min=0.61, max=1.91, step=0.02)
                
              ),
              tabPanel(
                "Costs",
                br(),
                h4("Genetic Testing Cost:"),
                numericInput("C_single_test",  "Single Test $", 100, 50, 300, 10),
                br(),
                
                h4("Drug Daily Cost:"),
                numericInput("C_clopidogrel",  "Clopidogrel $", 1, 0.1, 6.7, 0.1),
                numericInput("C_ticagrelor",  "Ticagrelor $", 7.3, 5, 10, 0.1),
                numericInput("C_prasugrel",  "Prasugrel $", 8.7, 5, 10, 0.1),
                numericInput("C_aspirin",  "Aspirin $", 0.13, 0.2, 0.3, 0.01)
              )
              
        ))),
        
        tabPanel(
          "Simvastatin",
          conditionalPanel(
            condition = "input.wDrug.includes('Simvastatin')",
            tabsetPanel(
              tabPanel(
                "Population",
                br(),
                h4("Phenotypic Prevalence:"),
                br(),
                sliderInput("vMedMetabolizer",  "Medium Metabolizer %",  value=0.249, min=0.15, max=1, step=0.01),
                sliderInput("vPoorMetabolizer", "Poor Metabolizer %", value=0.021, min=0.1, max=1, step=0.01)
                
              ),
              tabPanel(
                "Risks",
                br(),
                h4("Simvastatin:"),
                numericInput("vMildMyoSimNoVar",  "Mild Myopathy Baseline Risk",  value=0.05, min=0.01, max=0.2, step=0.01),
                numericInput("vMildMyoSimMedVar",  "Mild Myopathy RR for Medium Metabolizer",  value=2.55, min=1, max=10, step=0.1),
                numericInput("vMildMyoSimPoorVar", "Mild Myopathy RR for Poor Metabolizer", value=9.46, min=1, max=10, step=0.1),
                numericInput("vModMyoSimNoVar",  "Moderate Myopathy Baseline Risk",  value=0.00011, min=0.0001, max=0.2, step=0.0001),
                numericInput("vModMyoSimMedVar",  "Moderate Myopathy RR for Medium Metabolizer",  value=2.55, min=1, max=10, step=0.1),
                numericInput("vModMyoSimPoorVar", "Moderate Myopathy RR for Poor Metabolizer", value=9.46, min=1, max=10, step=0.1),
                numericInput("vSevMyoSimNoVar",  "Severe Myopathy Baseline Risk",  value=0.000034, min=0.00001, max=0.001, step=0.00001),
                numericInput("vSevMyoSimMedVar",  "Severe Myopathy RR for Medium Metabolizer",  value=2.55, min=1, max=10, step=0.1),
                numericInput("vSevMyoSimPoorVar", "Severe Myopathy RR for Poor Metabolizer", value=9.46, min=1, max=10, step=0.1),
                br(),
                
                h4("Alternative Drug:"),
                numericInput("vMildMyoAltNoVar",  "Mild Myopathy Baseline Risk",  value=0.05, min=0.01, max=0.2, step=0.01),
                numericInput("vMildMyoAltMedVar",  "Mild Myopathy RR for Medium Metabolizer",  value=1, min=1, max=10, step=0.1),
                numericInput("vMildMyoAltPoorVar", "Mild Myopathy RR for Poor Metabolizer", value=1, min=1, max=10, step=0.1),
                numericInput("vModMyoAltNoVar",  "Moderate Myopathy Baseline Risk",  value=0.00011, min=0.0001, max=0.2, step=0.0001),
                numericInput("vModMyoAltMedVar",  "Moderate Myopathy RR for Medium Metabolizer",  value=1.08, min=1, max=10, step=0.1),
                numericInput("vModMyoAltPoorVar", "Moderate Myopathy RR for Poor Metabolizer", value=4.05, min=1, max=10, step=0.1),
                numericInput("vSevMyoAltNoVar",  "Severe Myopathy Baseline Risk",  value=0.000034, min=0.00001, max=0.001, step=0.00001),
                numericInput("vSevMyoAltMedVar",  "Severe Myopathy RR for Medium Metabolizer",  value=1.08, min=1, max=10, step=0.1),
                numericInput("vSevMyoAltPoorVar", "Severe Myopathy RR for Poor Metabolizer", value=4.05, min=1, max=10, step=0.1),
                br(),
                
                h4("Other Risks:"),
                sliderInput("vProbRahbdoDeath",  "Case Fatality of Severe Myopathy",  value=0.1, min=0, max=1, step=0.01),
                sliderInput("vProbcvdDeath",  "Case Fatality of CVD",  value=0.117, min=0, max=1, step=0.01)
                
              ),
              
              tabPanel(
                "Costs",
                br(),
                h4("Genetic Testing Cost:"),
                numericInput("C_single_test",  "Single Test $", 100, 50, 300, 10))
        ))),
        
        tabPanel(
          "Warfarin",
          conditionalPanel(
            condition = "input.wDrug.includes('Warfarin')",
            tabsetPanel(
              tabPanel(
                "Population",
                br(),
                h4("AF Prevalence:"),
                br(),
                sliderInput("vpct_afib",  "AF %",  value=0.09, min=0, max=1, step=0.01)
                
              ),
              tabPanel(
                "Risks",
                br(),
                h4("Major Bleeding Events:"),
                numericInput("vAF_Risk_Major_Bleed_3",  "Risk among INR < 3 & AF Patients",  value=0.01497, min=0.01, max=0.2, step=0.005),
                numericInput("vAF_Risk_Major_Bleed_3to4",  "Risk among INR 3~4 & AF Patients",  value=0.06224, min=0.01, max=0.2, step=0.01),
                numericInput("vAF_Risk_Major_Bleed_Over4", "Risk among INR > 4 & AF Patients", value=0.39118, min=0.1, max=0.8, step=0.1),
                numericInput("vNonAF_Risk_Major_Bleed_3",  "Risk among INR < 3 & Non-AF Patients",  value=0.01497, min=0.01, max=0.2, step=0.005),
                numericInput("vNonAF_Risk_Major_Bleed_3to4",  "Risk among INR 3~4 & Non-AF Patients",  value=0.06224, min=0.01, max=0.2, step=0.01),
                numericInput("vNonAF_Risk_Major_Bleed_Over4", "Risk among INR > 4 & Non-AF Patients", value=0.39118, min=0.1, max=0.8, step=0.1),
                br(),
                
                h4("Minor Bleeding:"),
                numericInput("vAF_Risk_Minor_Bleed_3",  "Risk among INR < 3 & AF Patients",  value=0.0936, min=0.01, max=0.2, step=0.01),
                numericInput("vAF_Risk_Minor_Bleed_3to4",  "Risk among INR 3~4 & AF Patients",  value=0.3890, min=0.01, max=1, step=0.1),
                numericInput("vAF_Risk_Minor_Bleed_Over4", "Risk among INR > 4 & AF Patients", value=0.9999, min=0.1, max=1, step=0.1),
                numericInput("vNonAF_Risk_Minor_Bleed_3",  "Risk among INR < 3 & Non-AF Patients",  value=0.0936, min=0.01, max=0.2, step=0.01),
                numericInput("vNonAF_Risk_Minor_Bleed_3to4",  "Risk among INR 3~4 & Non-AF Patients",  value=0.3890, min=0.01, max=1, step=0.1),
                numericInput("vNonAF_Risk_Minor_Bleed_Over4", "Risk among INR > 4 & Non-AF Patients", value=0.9999, min=0.1, max=1, step=0.1),
                br(),
                
                h4("Stroke Events:"),
                numericInput("vAF_Risk_Stroke_1.5",  "Risk among INR < 1.5 & AF Patients",  value=0.077, min=0.01, max=0.2, step=0.01),
                numericInput("vAF_Risk_Stroke_1.5to2",  "Risk among INR 1.5~2 & AF Patients",  value=0.019, min=0.01, max=1, step=0.001),
                numericInput("vAF_Risk_Stroke_Over2", "Risk among INR > 2 & AF Patients", value=0.006, min=0.001, max=0.1, step=0.001),
                numericInput("vNonAF_Risk_Stroke_3",  "Risk among INR < 3 & Non-AF Patients",  value=0.00001, min=0.00001, max=0.1, step=0.01),
                numericInput("vNonAF_Risk_Stroke_Over3",  "Risk among INR > 3 & Non-AF Patients",  value=0.006, min=0.001, max=0.1, step=0.001),
                br(),                
                
                h4("DTVPE events, only among Non-AF patients:"),
                numericInput("vNonAF_Risk_DVTPE_2",  "Risk among INR < 1.5 & AF Patients",  value=0.077, min=0.01, max=0.2, step=0.01),
                numericInput("vNonAF_Risk_DVTPE_Over2",  "Risk among INR 1.5~2 & AF Patients",  value=0.019, min=0.01, max=1, step=0.001)
 
              ),
              
              tabPanel(
                "Costs",
                br(),
                h4("Genetic Testing Cost:"),
                numericInput("C_single_test",  "Single Test $", 100, 50, 300, 10),
                br(),
                h4("Drug Daily Cost:"),
                numericInput("C_warfarin",  "Warfarin $", 71/90, 0.5, 5, 0.1))
            )))
        
      )}})
  
  #copy and update inputs
  inputs_update <- eventReactive(input$run,{
    ip <- inputs
    ip$vN <- input$vN
    ip$vHorizon <- input$vHorizon
    ip$vReactive <- "None" 
    ip$whichDrug <- input$wDrug
    ip$iseed <- input$iseed
    ip$costs$single_test <- input$C_single_test
    
    if(any(input$wDrug == "Clopidogrel")) {
      ip$vDrugs$vClopidogrel <- TRUE
      ip$clopidogrel$vCYP2C19.Poor <- input$vCYP2C19.Poor
      ip$clopidogrel$vCYP2C19.Rapid <- input$vCYP2C19.Rapid
      ip$clopidogrel$vDAPT.SecondLine <- input$vDAPT.SecondLine
      ip$costs$clopidogrel <- input$C_clopidogrel
      ip$costs$aspirin <- input$C_aspirin
      ip$costs$ticagrelor <- input$C_ticagrelor
      ip$costs$prasugrel <- input$C_prasugrel
      ip$clopidogrel$vRiskST30 <- input$vRiskST30
      ip$clopidogrel$vRiskST365  <- input$vRiskST365
      ip$clopidogrel$vRiskSTgt365 <- input$vRiskSTgt365
      ip$clopidogrel$vRR.ST.Ticagrelor <- input$vRR.ST.Ticagrelor
      ip$clopidogrel$vRR.ST.Prasugrel <-  input$vRR.ST.Prasugrel
      ip$clopidogrel$vRR.ST.Aspirin  <-  input$vRR.ST.Aspirin
      ip$clopidogrel$vSt.Case.Fatality <-  input$vSt.Case.Fatality
      ip$clopidogrel$vPrCABG.ST <-  input$vPrCABG.ST
      ip$clopidogrel$vRiskMI <-  input$vRiskMI
      ip$clopidogrel$vRR.MI.Ticagrelor <-  input$vRR.MI.Ticagrelor
      ip$clopidogrel$vRR.MI.Prasugrel <-  input$vRR.MI.Prasugrel
      ip$clopidogrel$vRR.MI.Aspirin <-  input$vRR.MI.Aspirin
      ip$clopidogrel$vPrCABG.MI <-  input$vPrCABG.MI
      ip$clopidogrel$vPrPCI.MI <-  input$vPrPCI.MI
      ip$clopidogrel$vRiskRV365 <-  input$vRiskRV365
      ip$clopidogrel$vRiskRVgt365 <-  input$vRiskRVgt365
      ip$clopidogrel$vPrCABG.RV <-  input$vPrCABG.RV
      ip$clopidogrel$vRiskExtBleed <-  input$vRiskExtBleed
      ip$clopidogrel$vRiskIntBleed <-  input$vRiskIntBleed
      ip$clopidogrel$vRiskTIMIMinor <-  input$vRiskTIMIMinor
      ip$clopidogrel$vRiskFatalBleed <-  input$vRiskFatalBleed
      ip$clopidogrel$vRR.ExtBleed.Ticagrelor <-  input$vRR.ExtBleed.Ticagrelor
      ip$clopidogrel$vRR.ExtBleed.Prasugrel <-  input$vRR.ExtBleed.Prasugrel
      ip$clopidogrel$vRR.ExtBleed.Aspirin <-  input$vRR.ExtBleed.Aspirin
      ip$clopidogrel$vRR.IntBleed.Ticagrelor <-  input$vRR.IntBleed.Ticagrelor
      ip$clopidogrel$vRR.IntBleed.Prasugrel <-  input$vRR.IntBleed.Prasugrel
      ip$clopidogrel$vRR.IntBleed.Aspirin <-  input$vRR.IntBleed.Aspirin
      ip$clopidogrel$vRR.TIMIMinor.Ticagrelor <-  input$vRR.TIMIMinor.Ticagrelor
      ip$clopidogrel$vRR.TIMIMinor.Prasugrel <-  input$vRR.TIMIMinor.Prasugrel
      ip$clopidogrel$vRR.TIMIMinor.Aspirin <-  input$vRR.TIMIMinor.Aspirin
      ip$clopidogrel$vRR.FatalBleed.Ticagrelor <-  input$vRR.FatalBleed.Ticagrelor
      ip$clopidogrel$vRR.FatalBleed.Prasugrel <-  input$vRR.FatalBleed.Prasugrel
      ip$clopidogrel$vRR.FatalBleed.Aspirin <-  input$vRR.FatalBleed.Aspirin
      ip$clopidogrel$vRiskCABGTIMImajor <-  input$vRiskCABGTIMImajor
      ip$clopidogrel$vRR.RiskCABGTIMImajor.Ticagrelor <-  input$vRR.RiskCABGTIMImajor.Ticagrelor
      ip$clopidogrel$vRR.RiskCABGTIMImajor.Prasugrel <-  input$vRR.RiskCABGTIMImajor.Prasugrel
      ip$clopidogrel$vRR.RiskCABGTIMImajor.Aspirin <-  input$vRR.RiskCABGTIMImajor.Aspirin
      ip$clopidogrel$vRR.MI.LOF <-  input$vRR.MI.LOF
      ip$clopidogrel$vRR.Bleed.LOF  <-  input$vRR.Bleed.LOF
      ip$clopidogrel$vRR.ST.LOF <- input$vRR.ST.LOF
    } 
    
    if(any(input$wDrug=="Simvastatin"))  {
      ip$vDrugs$vSimvastatin <- TRUE
      ip$simvastatin$vMedMetabolizer  <- input$vMedMetabolizer
      ip$simvastatin$vPoorMetabolizer <- input$vPoorMetabolizer
      ip$simvastatin$vMildMyoSimNoVar <- input$vMildMyoSimNoVar
      ip$simvastatin$vMildMyoSimMedVar <- input$vMildMyoSimMedVar
      ip$simvastatin$vMildMyoSimPoorVar <- input$vMildMyoSimPoorVar
      ip$simvastatin$vMildMyoAltNoVar <- input$vMildMyoAltNoVar
      ip$simvastatin$vMildMyoAltMedVar <- input$vMildMyoAltMedVar
      ip$simvastatin$vMildMyoAltPoorVar <- input$vMildMyoAltPoorVar
      ip$simvastatin$vModMyoSimNoVar <- input$vModMyoSimNoVar
      ip$simvastatin$vModMyoSimMedVar <- input$vModMyoSimMedVar
      ip$simvastatin$vModMyoSimPoorVar <- input$vModMyoSimPoorVar
      ip$simvastatin$vModMyoAltNoVar <- input$vModMyoAltNoVar
      ip$simvastatin$vModMyoAltMedVar <- input$vModMyoAltMedVar
      ip$simvastatin$vModMyoAltPoorVar <- input$vModMyoAltPoorVar
      ip$simvastatin$vSevMyoSimNoVar <- input$vSevMyoSimNoVar
      ip$simvastatin$vSevMyoSimMedVar <- input$vSevMyoSimMedVar
      ip$simvastatin$vSevMyoSimPoorVar <- input$vSevMyoSimPoorVar
      ip$simvastatin$vSevMyoAltNoVar <- input$vSevMyoAltNoVar
      ip$simvastatin$vSevMyoAltMedVar <- input$vSevMyoAltMedVar
      ip$simvastatin$vSevMyoAltPoorVar <- input$vSevMyoAltPoorVar
      ip$simvastatin$vProbRahbdoDeath <- input$vProbRahbdoDeath
      ip$simvastatin$vProbcvdDeath <- input$vProbcvdDeath
    }
    
    if(any(input$wDrug == "Warfarin")) {
      ip$vDrugs$vWarfarin <- TRUE 
      ip$warfarin$vpct_afib <- input$vpct_afib
      ip$warfarin$vAF_Risk_Major_Bleed_3 <- input$vAF_Risk_Major_Bleed_3
      ip$warfarin$vAF_Risk_Major_Bleed_3to4 <- input$vAF_Risk_Major_Bleed_3to4
      ip$warfarin$vAF_Risk_Major_Bleed_Over4 <- input$vAF_Risk_Major_Bleed_Over4
      ip$warfarin$vNonAF_Risk_Major_Bleed_3 <- input$vNonAF_Risk_Major_Bleed_3
      ip$warfarin$vNonAF_Risk_Major_Bleed_3to4 <- input$vNonAF_Risk_Major_Bleed_3to4
      ip$warfarin$vNonAF_Risk_Major_Bleed_Over4 <- input$vNonAF_Risk_Major_Bleed_Over4
      ip$warfarin$vAF_Risk_Minor_Bleed_3 <- input$vAF_Risk_Minor_Bleed_3
      ip$warfarin$vAF_Risk_Minor_Bleed_3to4 <- input$vAF_Risk_Minor_Bleed_3to4
      ip$warfarin$vAF_Risk_Minor_Bleed_Over4 <- input$vAF_Risk_Minor_Bleed_Over4
      ip$warfarin$vNonAF_Risk_Minor_Bleed_3 <- input$vNonAF_Risk_Minor_Bleed_3
      ip$warfarin$vNonAF_Risk_Minor_Bleed_3to4 <- input$vNonAF_Risk_Minor_Bleed_3to4
      ip$warfarin$vNonAF_Risk_Minor_Bleed_Over4 <- input$vNonAF_Risk_Minor_Bleed_Over4
      ip$warfarin$vAF_Risk_Stroke_1.5 <- input$vAF_Risk_Stroke_1.5
      ip$warfarin$vAF_Risk_Stroke_1.5to2 <- input$vAF_Risk_Stroke_1.5to2
      ip$warfarin$vAF_Risk_Stroke_Over2 <- input$vAF_Risk_Stroke_Over2
      ip$warfarin$vNonAF_Risk_Stroke_3 <- input$vNonAF_Risk_Stroke_3
      ip$warfarin$vNonAF_Risk_Stroke_Over3 <- input$vNonAF_Risk_Stroke_Over3
      ip$warfarin$vNonAF_Risk_DVTPE_2 <- input$vNonAF_Risk_DVTPE_2
      ip$warfarin$vNonAF_Risk_DVTPE_Over2 <- input$vNonAF_Risk_DVTPE_Over2
      ip$costs$warfarin <- input$C_warfarin
      
    }
    
    return(ip)
  })
  
  #base case results
  base <- eventReactive(input$run,{
      inputs <- inputs_update()
      out <- exec.simulation(inputs)
      out$sm <- out$sm %>% arrange(num) %>% dplyr::select(Event,Count)
      return(out)
  })
  
  #None vs Reactive results
  add <- eventReactive(input$run,{
    
  if(input$wTest =="Single") {
    inputs <- inputs_update()
    inputs$vReactive <- "Single"  

    add <- exec.simulation(inputs)
    names(add$sm)[2] <- "Genotyping"
    add$sum_costs$Strategy <- "Genotyping"

    return(add)
    } else {NULL}
    })
  
  #combine and format results
  out <- eventReactive(input$run,{
    full <- list()
    if(input$wTest=="Single") {
      ne <- base()$sm
      names(ne)[2] <- "None" 
      full$sm <- merge(ne,add()$sm,by="Event") %>% arrange(num) %>% dplyr::select(-num)
      
      nc <- base()$sum_costs
      nc$Strategy <- "None"
      full$sum_costs <- rbind(nc,add()$sum_costs) %>% mutate(ICER = (dCOST[1]-dCOST)/(dQALY[1]-dQALY))
      full$sum_costs <- cbind(full$sum_costs$Strategy,full$sum_costs %>% dplyr::select(-Strategy))
      full$sum_costs$dQALY <- sprintf("%.5f",full$sum_costs$dQALY)
      names(full$sum_costs) <- c("Strategy","QALY","Total Cost","Test Cost","Drug Cost","AE Cost","ICER to None")

    } else {
      full$sm <- base()$sm
      full$sum_costs <- base()$sum_costs
      full$sum_costs$dQALY <- sprintf("%.5f",full$sum_costs$dQALY)
      names(full$sum_costs) <- c("QALY","Total Cost","Test Cost","Drug Cost","AE Cost")
      }
    
    return(full)
  })
  
  save_dt <- eventReactive(input$saveButton,{
    out()
  }) 
  
  output$events <- renderDataTable({
    out()$sm
    }, escape=FALSE)
  
  output$costs <- renderTable({
    out()$sum_costs     
  }, rownames = FALSE, digits=1, bordered=FALSE)
  
  output$last_e <- renderDataTable({
    save_dt()$sm
  }, escape=FALSE)
  
  output$last_c <- renderTable({
    save_dt()$sum_costs
  }, rownames = FALSE, digits=1, bordered=FALSE)
  
})
