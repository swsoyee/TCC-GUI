library(shiny)
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
source(file = "global.R",
       local = TRUE,
       encoding = "UTF-8")


# Define server
shinyServer(function(input, output, session) {
  variables = reactiveValues(
    CountData = data.frame("Load your data first." = character(0)),
    groupList = "",
    groupListConvert = "",
    runTimes = 0,
    result = data.frame("Results will show here." = character(0)),
    norData = "",
    runTCCCode = "",
    runMAPlot = "",
    runVolcanoPlot = "",
    runHeatmap = "",
    runPCACode = "",
    logList = data.frame("Time" = vector(), 
                         "Type" = vector(),
                         "Action" = vector(), 
                         "Parameters" = vector())
  )
  
  source(file = "server-data-import.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-tcc-calculation.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-ma-plot.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-volcano-plot.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-pca.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-tabPanel.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-heatmap.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-expression-plot.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-report.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "R-code.R",
         local = TRUE,
         encoding = "UTF-8")
  
})
