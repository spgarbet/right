library(shiny)

parameterInput <- function(inputId, label, value, min=NA, max=NA, step=NA, width=NULL)
{
  fluidRow(column(width=5, HTML(label)),
           column(width=7, numericInput(inputId, NA, value, min, max, step)))
  #parameterInput(inputId, label, value, min, max, step, "60%")
}


shinyUI(fluidPage(
  
  #adjust diagrams width
  tags$head(tags$style(
    type="text/css",
    "#image1 img {max-width: 100%; width: 100%; height: auto}",
    "#image2 img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  titlePanel("Clopidogrel Simulation"),
  p("A discrete event simulation model for evaluation of clinical benefit and costs-effectiveness of utilizing pharmacogenomic testing in treatement"),
  
  #input panel
  sidebarLayout(
    sidebarPanel(
      width=6,
      actionButton("run","Run"),
      actionButton("saveButton","Save"),
      h3("General Settings"),
      br(),
      
      numericInput("vN", "Sample Size:", min = 0, max = 100000, value = 500, step=50),
      br(),
      
      sliderInput("vHorizon","Time Horizon (Year):", min=1, max=80, value=10, step=1),
      br(),
      
      #selectInput("wDrug","Which model?",c("Clopidogrel","Simvastatin"),"None",FALSE),
      checkboxGroupInput("wDrug","Which model?",c("Clopidogrel","Simvastatin"),selected = "Clopidogrel"),
      br(),
      
      h5("Genotyping Strategy (by default no testing):"),
      checkboxInput("test","genotyping"),
      br(),
      
      numericInput("iseed", "Random number seed:", min = 1, max = 100000, value = 12345, step=1),
      br(),
      
      h3("Model-Specific Parameters"),
      uiOutput("uim")),
      
    #output panel
    mainPanel(
      width=6,
      
      
      tabsetPanel(
        tabPanel(
          "Simulation Results",
          h3("Event Counts"),
          dataTableOutput("events"),
          br(),
          
          h3("Average Costs & QALYs"),
          tableOutput("costs"),
          
          h3("Last Saved Results"),
          dataTableOutput("last_e"),
          tableOutput("last_c")
        ),
        tabPanel(
          "Simulation Method",
          h3("Clopidogrel"),
          img(src="clopidogrel_diagram.png", width="100%"),
          br(),
          h3("Simvastatin"),
          img(src="simvastatin_diagram.png", width="100%")
        )
        
      ))),


      
  p("Built using ",
    a(href="https://www.r-project.org/", img(src="Rlogo.png", alt="R", width="60")),
    "with the ",
    a(href="https://cran.r-project.org/web/packages/simmer/index.html", "simmer"),
    " and ",
    a(href="https://cran.r-project.org/web/packages/shiny/shiny.pdf", "shiny"),
    "packages."
  )
  #p("TODO: Website footer here")
  )
)
