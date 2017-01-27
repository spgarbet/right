library(shiny)

source("run.R")
source("costs.R")

shinyServer(function(input, output) {
  #update inputs (simulation) with input (ui values)
  base <- eventReactive(input$run,{
      #simulation setting
      inputs$iseed <- input$iseed
      inputs$vN <- input$vN
      inputs$vHorizon <- input$vHorizon
      inputs$clopidogrel$vCYP2C19.Poor <- input$vCYP2C19.Poor
      inputs$clopidogrel$vCYP2C19.Rapid <- input$vCYP2C19.Rapid
      #inputs$vCYP2C19.Unknown <- input$vCYP2C19.Unknown
      #inputs$clopidogrel$vPREDICTsens <- input$vPREDICTsens
      #inputs$clopidogrel$vPREDICTspec <- input$vPREDICTspec
      inputs$clopidogrel$vDAPT.SecondLine <- input$vDAPT.SecondLine
      inputs$vReactive <- "None" 
      
      inputs$costs$clopidogrel <- input$C_clopidogrel
      inputs$costs$aspirin <- input$C_aspirin
      inputs$costs$ticagrelor <- input$C_ticagrelor
      inputs$costs$prasugrel <- input$C_prasugrel
      
      out <- exec.simulation(inputs)
      out$summary <- out$summary %>% arrange(num) %>% select(Event,Count)
      return(out)
  })

  add <- eventReactive(input$run,{
    
  #if(length(input$vStrategy)>0) {
  if(input$test) {
    inputs$iseed <- input$iseed
    inputs$vN <- input$vN
    inputs$vHorizon <- input$vHorizon
    inputs$clopidogrel$vCYP2C19.Poor <- input$vCYP2C19.Poor
    inputs$clopidogrel$vCYP2C19.Rapid <- input$vCYP2C19.Rapid
    #inputs$vCYP2C19.Unknown <- input$vCYP2C19.Unknown
    #inputs$clopidogrel$vPREDICTsens <- input$vPREDICTsens
    #inputs$clopidogrel$vPREDICTspec <- input$vPREDICTspec
    inputs$clopidogrel$vDAPT.SecondLine <- input$vDAPT.SecondLine
    inputs$costs$single_test <- input$C_single_test
    inputs$vReactive <- "Single"  
    
    inputs$costs$clopidogrel <- input$C_clopidogrel
    inputs$costs$aspirin <- input$C_aspirin
    inputs$costs$ticagrelor <- input$C_ticagrelor
    inputs$costs$prasugrel <- input$C_prasugrel
    
    add <- exec.simulation(inputs)
    names(add$summary)[2] <- "Genotyping"
    add$sum_costs$Strategy <- "Genotyping"
    
    #add <- NULL
    #for(i in 1:length(input$vStrategy)) {
     #inputs$vPreemptive <- trans_strategy(input$vStrategy[i])$preemptive
     #inputs$vReactive <- trans_strategy(input$vStrategy[i])$reactive  
     #run <- exec.simulation(inputs)
     #names(run$summary)[2] <- paste(input$vStrategy[i]) #name event count 
     #run$sum_costs$Strategy <- paste(input$vStrategy[i]) #name costs
     
     #if(is.null(add)) { 
      # add$summary <- run$summary 
      # add$sum_costs <- run$sum_costs
     #} else {
      # add$summary <- merge(add$summary, run$summary, by=c("Event","num"))
      # add$sum_costs <- rbind(add$sum_costs,run$sum_costs)
      # }
    #} 
    return(add)
    } else {NULL}
    })
  
  out <- eventReactive(input$run,{
    full <- list()
    if(input$test) {
    #if(length(input$vStrategy)>0) {
      ne <- base()$summary
      names(ne)[2] <- "None" 
      full$summary <- merge(ne,add()$summary,by="Event") %>% arrange(num) %>% select(-num)
      
      nc <- base()$sum_costs
      nc$Strategy <- "None"
      full$sum_costs <- rbind(nc,add()$sum_costs) %>% mutate(ICER = (dCOST[1]-dCOST)/(dQALY[1]-dQALY))
      full$sum_costs <- cbind(full$sum_costs$Strategy,full$sum_costs %>% select(-Strategy))
      full$sum_costs$dQALY <- sprintf("%.5f",full$sum_costs$dQALY)
      names(full$sum_costs) <- c("Strategy","QALY","Total Cost","Test Cost","Drug Cost","AE Cost","ICER to None")

    } else {
      full$summary <- base()$summary
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
    out()$summary
    }, escape=FALSE)
  
  output$costs <- renderTable({
    out()$sum_costs     
  }, rownames = FALSE, digits=1, bordered=FALSE)
  
  output$last_e <- renderTable({
    save_dt()$summary
  }, rownames = FALSE, digits=0, bordered=FALSE)
  
  output$last_c <- renderTable({
    save_dt()$sum_costs
  }, rownames = FALSE, digits=1, bordered=FALSE)
  
})
