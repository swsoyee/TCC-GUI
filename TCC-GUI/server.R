library(shiny)
source(file = "global.R", local = TRUE, encoding = "UTF-8")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  variables = reactiveValues(CountData = data.frame("Load your data first." = character(0)),
                             groupList = "",
                             runTimes = 0,
                             result = data.frame("Results will show here." = character(0)),
                             norData = "",
                             runTCCCode = ""
                             )
  
  source(file = "server-data-import.R", local = TRUE, encoding = "UTF-8")
  source(file = "server-tcc-calculation.R", local = TRUE, encoding = "UTF-8")
  source(file = "server-ma-plot.R", local = TRUE, encoding = "UTF-8")
  source(file = "server-volcano-plot.R", local = TRUE, encoding = "UTF-8")
  source(file = "server-tabPanel.R", local = TRUE, encoding = "UTF-8")
  source(file = "R-code.R", local = TRUE, encoding = "UTF-8")
  
})
