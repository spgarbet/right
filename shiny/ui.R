library(shiny)

parameterInput <- function(inputId, label, value, min=NA, max=NA, step=NA, width=NULL)
{
  fluidRow(column(width=5, HTML(label)),
           column(width=7, numericInput(inputId, NA, value, min, max, step)))
  #parameterInput(inputId, label, value, min, max, step, "60%")
}


shinyUI(fluidPage(
  titlePanel("Clopidogrel Simulation"),
  p("A discrete event simulation model for evaluation of clinical benefit and costs-effectiveness of utilizing pharmacogenomic testing in treatement"),
  
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
      
      selectInput("wDrug","Which model?",c("Clopidogrel","Simvastatin"),"None",FALSE),
      br(),
      
      h4("Genotyping Strategy (by default no testing):"),
      checkboxInput("test","genotyping"),
      br(),
      
      numericInput("iseed", "Random number seed:", min = 1, max = 100000, value = 12345, step=1),
      br(),
      
      h3("Model-Specific Parameters"),
      uiOutput("uiClo"),
      uiOutput("uiSim")
    ),

    mainPanel(
      width=6,
      
      h2("Simulation Results"),
      h3("Event Counts"),
      dataTableOutput("events"),
      br(),
      
      h3("Average Costs & QALYs"),
      tableOutput("costs"),
      
      h3("Last Saved Results"),
      dataTableOutput("last_e"),
      tableOutput("last_c"),
      
      h3("Simulation Method"),
      img(src="clopidogrel_diagram.png", width="100%")
    )),
      
  p("Built using ",
    a(href="https://www.r-project.org/", img(src="Rlogo.png", alt="R", width="60")),
    "with the ",
    a(href="https://cran.r-project.org/web/packages/simmer/index.html", "simmer"),
    " and ",
    a(href="https://cran.r-project.org/web/packages/shiny/shiny.pdf", "shiny"),
    "packages."
  )#,
  #p("TODO: Website footer here")
  )
)
