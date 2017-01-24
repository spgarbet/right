library(shiny)

parameterInput <- function(inputId, label, value, min=NA, max=NA, step=NA, width=NULL)
{
  fluidRow(column(width=5, HTML(label)),
           column(width=7, numericInput(inputId, NA, value, min, max, step)))
  #parameterInput(inputId, label, value, min, max, step, "60%")
}


shinyUI(fluidPage(
  titlePanel("Clopidogrel Simulation"),
  p("A discrete event simulation model for evaluation of clinical benefit and costs-effectiveness of utilizing pharmacogenomic testing in Simvastatin treatement"),
  
  sidebarLayout(
    sidebarPanel(
      width=6,
      submitButton("Run"),                 
      h3("Parameters"),
      tabsetPanel(type="tabs",
        tabPanel(
          "Simulation",
          br(),
          sliderInput("vN","Sample size:",min=10,max=200,value=10,step=10),
          sliderInput("vHorizon","Time Horizon", min=1, max=80, value=10, step=1),
          br(),
          h5("Genotyping strategy"),
          selectInput("vPreemptive", "Preemptive",
                      c("None", "Panel", "PREDICT", "Age >= 50"), "None", FALSE),
          selectInput("vReactive", "Reactive",
                      c("None", "Single", "Panel"), "None", FALSE),
          br(),
          br(),
          selectInput("vDAPT.SecondLine", "DAPT: Alt Drug",
                      c("Ticagrelor", "Prasugrel"), "None", FALSE)
          ),
        tabPanel(
          "Population",
          h5("Age"),
          br(),
          h5("Phenotypic Prevalence (Clopidogrel: CYP2C19)"),
          parameterInput("vCYP2C19.Poor",  "Poor %",  0.21, 0.15, 0.4, 0.01),
          parameterInput("vCYP2C19.Rapid", "Rapid %", 0.33, 0.1, 0.4, 0.01),
          parameterInput("vCYP2C19.Unknown", "Unknown %", 0.07, 0.05, 0.09, 0.01)
        ),
        tabPanel(
          "Costs",
          h5("Genetic testing"),
          parameterInput("C_single_test",  "Single Test $", 100, 50, 300, 10),
          parameterInput("C_panel_test",  "Panel Test $", 250, 100, 500, 10)

        ))
    ),

###other inputs:
#parameterInput("vPREDICTsens", "PREDICT Sensitivity", min=0, max=1, value=0.3, step=0.1),
#parameterInput("vPREDICTspec", "PREDICT Specificity", min=0, max=1, value=0.3, step=0.1)
        
    mainPanel(
      width=6,
      h2("Simulation Results"),
      h3("Event Counts"),
      tableOutput("sumct"),
      h2(""),
      
      h3("Average Costs & QALYs"),
      tableOutput("sumcq"),
      
      h3("Simulation Method"),
      img(src="SimvastatinMethod.png", width="60%")
    )),
      
  p("Built using ",
    a(href="https://www.r-project.org/", img(src="Rlogo.png", alt="R", width="60")),
    "with the ",
    a(href="https://cran.r-project.org/web/packages/simmer/index.html", "simmer"),
    " and ",
    a(href="https://cran.r-project.org/web/packages/shiny/shiny.pdf", "shiny"),
    "packages."
  ),
  p("TODO: Website footer here")
  )
)
