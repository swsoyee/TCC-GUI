library(shiny)
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
source(file = "global.R",
       local = TRUE,
       encoding = "UTF-8")


# Define server
shinyServer(function(input, output, session) {
  variables = reactiveValues(
    simulationData = "",
    CountData = data.frame(),
    groupList = NULL,
    groupListConvert = NULL,
    result = data.frame("Results will show here." = character(0)),
    zeroValue = "",
    norData = "",
    runTCCCode = "",
    runMAPlot = "",
    runVolcanoPlot = "",
    runHeatmap = "",
    runPCACode = "",
    sampleDistributionBar = "",
    sampleDistributionDensity = "",
    norSampleDistributionBar = "",
    norSampleDistributionDensity = "",
    MAPlotObject = "",
    VolcanoPlotObject = "",
    screePlot = "",
    pca3d = "",
    pca2d = "",
    heatmapObject = "",
    expressionLevelBar = "",
    expressionLevelBox = "",
    logList = data.frame(
      "Time" = vector(),
      "Type" = vector(),
      "Action" = vector(),
      "Parameters" = vector()
    )
  )
  source(file = "server-simulation.R",
         local = TRUE,
         encoding = "UTF-8")
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
