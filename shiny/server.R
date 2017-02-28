library(shiny)

source("run.R")
source("costs.R")

shinyServer(function(input, output) {

  output$uiClo <- renderUI({
    if(input$wDrug=="Clopidogrel") {
      tabsetPanel(type="tabs",
                  
                  tabPanel(
                    "Strategy",
                    br(),
                    selectInput("vDAPT.SecondLine", "DAPT: Alternative Drug",
                                c("Ticagrelor", "Prasugrel"), "None", FALSE)
                  ),
                  
                  tabPanel(
                    "Population",
                    br(),
                    h4("Phenotypic Prevalence:"),
                    br(),
                    sliderInput("vCYP2C19.Poor",  "Poor or Intermediate Metabolizer %",  value=0.21, min=0.15, max=1, step=0.01),
                    sliderInput("vCYP2C19.Rapid", "Rapid Metabolizer %", value=0.33, min=0.1, max=1, step=0.01)
                    #sliderInput("vCYP2C19.Unknown", "Unknown %", value=0.07, min=0.05, max=0.09, step=0.01)
                    #sliderInput vs parameter/numeric input: need to limit bounds?, should not sum up over 1????
                  ),
                  
                  tabPanel(
                    "Risks",
                    br()
                    #sliderInput("vCYP2C19.Poor",  "Poor or Intermediate Metabolizer %",  value=0.21, min=0.15, max=1, step=0.01),
                    #sliderInput("vCYP2C19.Rapid", "Rapid Metabolizer %", value=0.33, min=0.1, max=1, step=0.01)
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
                  ))}
  })
  
  output$uiSim <- renderUI({
    if(input$wDrug=="Simvastatin") {
      tabsetPanel(type="tabs",
                  
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
                    h3("Clopidogrel:"),
                    h4("Mild Myopathy"),
                    numericInput("vMildMyoSimNoVar",  "Baseline Risk",  value=0.05, min=0.01, max=0.2, step=0.01),
                    numericInput("vMildMyoSimMedVar",  "RR for Medium Metabolizer",  value=2.55, min=1, max=10, step=0.1),
                    numericInput("vMildMyoSimPoorVar", "RR for Poor Metabolizer", value=9.46, min=1, max=10, step=0.1),
                    h4("Moderate Myopathy"),
                    numericInput("vModMyoSimNoVar",  "Baseline Risk",  value=0.00011, min=0.0001, max=0.2, step=0.0001),
                    numericInput("vModMyoSimMedVar",  "RR for Medium Metabolizer",  value=2.55, min=1, max=10, step=0.1),
                    numericInput("vModMyoSimPoorVar", "RR for Poor Metabolizer", value=9.46, min=1, max=10, step=0.1),
                    h4("Severe Myopathy"),
                    numericInput("vSevMyoSimNoVar",  "Baseline Risk",  value=0.000034, min=0.00001, max=0.001, step=0.00001),
                    numericInput("vSevMyoSimMedVar",  "RR for Medium Metabolizer",  value=2.55, min=1, max=10, step=0.1),
                    numericInput("vSevMyoSimPoorVar", "RR for Poor Metabolizer", value=9.46, min=1, max=10, step=0.1),
                    br(),
                    
                    h3("Alternative Drug:"),
                    h4("Mild Myopathy"),
                    numericInput("vMildMyoAltNoVar",  "Baseline Risk",  value=0.05, min=0.01, max=0.2, step=0.01),
                    numericInput("vMildMyoAltMedVar",  "RR for Medium Metabolizer",  value=1, min=1, max=10, step=0.1),
                    numericInput("vMildMyoAltPoorVar", "RR for Poor Metabolizer", value=1, min=1, max=10, step=0.1),
                    h4("Moderate Myopathy"),
                    numericInput("vModMyoAltNoVar",  "Baseline Risk",  value=0.00011, min=0.0001, max=0.2, step=0.0001),
                    numericInput("vModMyoAltMedVar",  "RR for Medium Metabolizer",  value=1.08, min=1, max=10, step=0.1),
                    numericInput("vModMyoAltPoorVar", "RR for Poor Metabolizer", value=4.05, min=1, max=10, step=0.1),
                    h4("Severe Myopathy"),
                    numericInput("vSevMyoAltNoVar",  "Baseline Risk",  value=0.000034, min=0.00001, max=0.001, step=0.00001),
                    numericInput("vSevMyoAltMedVar",  "RR for Medium Metabolizer",  value=1.08, min=1, max=10, step=0.1),
                    numericInput("vSevMyoAltPoorVar", "RR for Poor Metabolizer", value=4.05, min=1, max=10, step=0.1),
                    br(),
                    
                    h3("Other Risks:"),
                    sliderInput("vProbRahbdoDeath",  "Case Fatality of Severe Myopathy",  value=0.1, min=0, max=1, step=0.01),
                    sliderInput("vProbcvdDeath",  "Case Fatality of CVD",  value=0.117, min=0, max=1, step=0.01)
                    
                  ),
                  
                  tabPanel(
                    "Costs",
                    br(),
                    h4("Genetic Testing Cost:"),
                    numericInput("C_single_test",  "Single Test $", 100, 50, 300, 10)
                  ))}
  })  
  
  
  
  #copy and update inputs
  inputs_update <- eventReactive(input$run,{
    ip <- inputs
    ip$vN <- input$vN
    ip$vHorizon <- input$vHorizon
    ip$vReactive <- "None" 
    ip$whichDrug <- input$wDrug
    ip$vDrugs <- trans_model(input$wDrug)
    ip$iseed <- input$iseed
    ip$costs$single_test <- input$C_single_test
    if(input$wDrug=="Clopidogrel") {
      ip$clopidogrel$vCYP2C19.Poor <- input$vCYP2C19.Poor
      ip$clopidogrel$vCYP2C19.Rapid <- input$vCYP2C19.Rapid
      ip$clopidogrel$vDAPT.SecondLine <- input$vDAPT.SecondLine
      ip$costs$clopidogrel <- input$C_clopidogrel
      ip$costs$aspirin <- input$C_aspirin
      ip$costs$ticagrelor <- input$C_ticagrelor
      ip$costs$prasugrel <- input$C_prasugrel
      
    } else if(input$wDrug=="Simvastatin")  {
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
    return(ip)
  })
  
  #base case results
  base <- eventReactive(input$run,{
      inputs <- inputs_update()
      out <- exec.simulation(inputs)
      out$sm <- out$sm %>% arrange(num) %>% select(Event,Count)
      return(out)
  })
  
  #None vs Reactive results
  add <- eventReactive(input$run,{
    
  if(input$test) {
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
    if(input$test) {
      ne <- base()$sm
      names(ne)[2] <- "None" 
      full$sm <- merge(ne,add()$sm,by="Event") %>% arrange(num) %>% select(-num)
      
      nc <- base()$sum_costs
      nc$Strategy <- "None"
      full$sum_costs <- rbind(nc,add()$sum_costs) %>% mutate(ICER = (dCOST[1]-dCOST)/(dQALY[1]-dQALY))
      full$sum_costs <- cbind(full$sum_costs$Strategy,full$sum_costs %>% select(-Strategy))
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
