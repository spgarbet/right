library(shiny)

setwd("/Users/zilu/Desktop/right-simulation")
source("./shiny/run.R")
source("./shiny/costs.R")

shinyServer(function(input, output) {
  #update inputs (simulation) with input (ui values)
  results <- reactive({
      inputs$vN = input$vN
      inputs$vHorizon = input$vHorizon
      inputs$vCYP2C19.Poor = input$vCYP2C19.Poor
      inputs$vCYP2C19.Rapid = input$vCYP2C19.Rapid
      inputs$vCYP2C19.Unknown = input$vCYP2C19.Unknown
      inputs$vPreemptive = input$vPreemptive
      inputs$vReactive = input$vReactive
      #inputs$vPREDICTsens = input$vPREDICTsens
      #inputs$vPREDICTspec = input$vPREDICTspec
      inputs$clopidogrel$vDAPT.SecondLine = input$vDAPT.SecondLine
      inputs$costs$single_test = input$C_single_test
      inputs$costs$panel_test = input$C_panel_test
      
      exec.simulation(inputs)
  })
  
  output$sumct <- renderTable({
    results()$summary
  },include.rownames = FALSE)
  output$sumcq <- renderTable({
    results()$sum_costs
  },include.rownames = FALSE)
  
})
