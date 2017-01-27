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
      actionButton("run","Run"),
      actionButton("saveButton","Save"),
      h3("Parameters"),
      tabsetPanel(type="tabs",
        tabPanel(
          "Simulation",
          br(),
          
          #sliderInput("vN","Sample size:",min=10,max=10000,value=10,step=10),
          numericInput("vN", "Sample Size", min = 0, max = 100000, value = 500, step=50),
          br(),
          
          sliderInput("vHorizon","Time Horizon (Year)", min=1, max=80, value=10, step=1),
          br(),
          
          #selectizeInput("vStrategy", "Genotyping strategy to compare", 
          #               choices=c("Reactive", "Universal Preemptive","Age >= 50","Targeted Preemptive"),
          #               multiple=TRUE),
          h5("Genotyping Strategy (by default no testing)"),
          checkboxInput("test","CYP2C19 genotyping"),
          br(),
          
          selectInput("vDAPT.SecondLine", "DAPT: Alt Drug",
                      c("Ticagrelor", "Prasugrel"), "None", FALSE),
          br(),
          numericInput("iseed", "Random number seed:", min = 1, max = 100000, value = 12345, step=1)
          ),
        tabPanel(
          "Population",
          #h5("Age"),
          br(),
          h4("Phenotypic Prevalence (Clopidogrel: CYP2C19)"),
          br(),
          sliderInput("vCYP2C19.Poor",  "Poor or Intermediate Metabolizer %",  value=0.21, min=0.15, max=1, step=0.01),
          sliderInput("vCYP2C19.Rapid", "Rapid Metabolizer %", value=0.33, min=0.1, max=1, step=0.01)
          #sliderInput("vCYP2C19.Unknown", "Unknown %", value=0.07, min=0.05, max=0.09, step=0.01)
          #sliderInput vs parameter/numeric input: need to limit bounds?, should not sum up over 1????
        ),
        #tabPanel(
          #"Customized Prediction Algorithm",
          #br(),
          #parameterInput("vPREDICTsens", "Sensitivity", min=0, max=1, value=0.23, step=0.1),
          #parameterInput("vPREDICTspec", "Specificity", min=0, max=1, value=0.93, step=0.1),
          #h5("Sensitivity and specificity should match algorithm at selected time horizon.")
        #),
        tabPanel(
          "Costs",
          br(),
          h4("Genetic Testing Cost"),
          parameterInput("C_single_test",  "Single Test $", 100, 50, 300, 10),
          #parameterInput("C_panel_test",  "Panel Test $", 250, 100, 500, 10)
          br(),
          
          h4("Drug Daily Cost"),
          parameterInput("C_clopidogrel",  "Clopidogrel $", 1, 0.1, 6.7, 0.1),
          parameterInput("C_ticagrelor",  "Ticagrelor $", 7.3, 5, 10, 0.1),
          parameterInput("C_prasugrel",  "Prasugrel $", 8.7, 5, 10, 0.1),
          parameterInput("C_aspirin",  "Aspirin $", 0.13, 0.2, 0.3, 0.01)

        ))
    ),

###other inputs:
#parameterInput("vPREDICTsens", "PREDICT Sensitivity", min=0, max=1, value=0.3, step=0.1),
#parameterInput("vPREDICTspec", "PREDICT Specificity", min=0, max=1, value=0.3, step=0.1)
        
    mainPanel(
      width=6,
      
      verbatimTextOutput("sele"),
      tableOutput("add"),
      
      h2("Simulation Results"),
      h3("Event Counts"),
      dataTableOutput("events"),
      br(),
      
      h3("Average Costs & QALYs"),
      tableOutput("costs"),
      
      h3("Last Saved Results"),
      tableOutput("last_e"),
      tableOutput("last_c"),
      
      h3("Simulation Method"),
      img(src="Clopidogrel_diagram.png", width="85%")
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
