library(shiny)
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
source(file = "global.R",
       local = TRUE,
       encoding = "UTF-8")


# Define server
shinyServer(function(input, output, session) {
  variables = reactiveValues(
    simulationData = NULL,
    count.data = NULL,
    CountData = data.frame(),
    groupList = NULL,
    groupListConvert = NULL,
    result = data.frame("Results will show here." = character(0)),
    tccObject = NULL,
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
    
    mdsPlot = list(),
    mdsPlotplot = NULL,
      
    data.pca = NULL,
    pcaParameter = NULL,
    screePlot = NULL,
    pca3d = NULL,
    pca2d = NULL,
    summaryPCA = NULL,
    
    heatmapObject = "",
    
    expressionData = NULL,
    expressionLevelBar = NULL,
    expressionLevelBox = NULL,
    expressionLevelCountTable = NULL,
    expressionLevelResultTable = NULL,
    
    logList = data.frame(
      "Time" = vector(),
      "Type" = vector(),
      "Action" = vector(),
      "Parameters" = vector()
    ),
    reportFile = NULL
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
