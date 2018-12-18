# server-report.R

runReport <- reactiveValues(runReportValue = FALSE)
# If analysis part has been executed, than render check box ----
output$renderSimulationReportOption <- renderUI({
  if(simuRun$simulationRunValue){
    awesomeCheckboxGroup(
      inputId = "simulationReportOption",
      label = "Simulation Data",
      choices = c("Parameters", "Summary Table"),
      selected = c("Parameters", "Summary Table"),
      inline = TRUE
    )
  } else {
    tagList(
      tags$b("Simulation Data"),
      helpText("You need to execute this part first to generate reportable objects.")
    )
  }
})

output$renderMaReportOption <- renderUI({
  if(runMA$runMAValues){
    awesomeCheckboxGroup(
      inputId = "maReportOption",
      label = "MA Plot",
      choices = c("Parameters", "MA Plot"),
      selected = c("Parameters", "MA Plot"),
      inline = TRUE
    )
  } else {
    tagList(
      tags$b("MA Plot"),
      helpText("You need to execute this part first to generate reportable objects.")
    )
  }
})

output$renderVolcanoReportOption <- renderUI({
  if(runVolcano$runVolcanoValue){
    awesomeCheckboxGroup(
      inputId = "volcanoReportOption",
      label = "Volcano Plot",
      choices = c("Parameters", "Volcano Plot"),
      selected = c("Parameters", "Volcano Plot"),
      inline = TRUE
    )
  } else {
    tagList(
      tags$b("Volcano Plot"),
      helpText("You need to execute this part first to generate reportable objects.")
    )
  }
})

output$renderHeatmapReportOption <- renderUI({
  if(runHeatmap$runHeatmapValue){
    awesomeCheckboxGroup(
      inputId = "heatmapReportOption",
      label = "Heatmap",
      choices = c("Parameters", "Data Table", "Heatmap"),
      selected = c("Parameters", "Data Table", "Heatmap"),
      inline = TRUE
    )
  } else {
    tagList(
      tags$b("Heatmap"),
      helpText("You need to execute this part first to generate reportable objects.")
    )
  }
})

output$renderExpressionReportOption <- renderUI({
  if(runExp$runExpValue){
    awesomeCheckboxGroup(
      inputId = "expressionReportOption",
      label = "Expression Level",
      choices = c("Parameters", "Data Table", "Barplot", "Boxplot"),
      selected = c("Parameters", "Data Table", "Barplot", "Boxplot"),
      inline = TRUE
    )
  } else {
    tagList(
      tags$b("Expression Level"),
      helpText("You need to execute this part first to generate reportable objects.")
    )
  }
})
# Render all check box in the report option ----
output$reportOption <- renderUI({
  tagList(
    uiOutput("renderSimulationReportOption"),
    tags$hr(),
    awesomeCheckboxGroup(
      inputId = "importReportOption",
      label = "Exploratory Analysis",
      choices = c(
        "Parameters",
        "Summary Table",
        "Count Distribution",
        "Filtering Threshold",
        "Density Plot",
        "MDS Plot",
        "PCA Summary Table", 
        "PCA Scree Plot", 
        "PCA 3D Plot", 
        "PCA 2D Plot",
        "Hierarchical Clustering"
      ),
      selected = c(
        "Parameters",
        "Summary Table",
        "Count Distribution",
        "Filtering Threshold",
        "Density Plot",
        "MDS Plot",
        "PCA Summary Table", 
        "PCA Scree Plot", 
        "PCA 3D Plot", 
        "PCA 2D Plot",
        "Hierarchical Clustering"
      ),
      inline = TRUE
    ),
    tags$hr(),
    awesomeCheckboxGroup(
      inputId = "tccReportOption",
      label = "TCC Computation",
      choices = c("Parameters", "Code", "Summary Table"),
      selected = c("Parameters", "Summary Table"),
      inline = TRUE
    ),
    tags$hr(),
    uiOutput("renderMaReportOption"),
    tags$hr(),
    uiOutput("renderVolcanoReportOption"),
    tags$hr(),
    uiOutput("renderHeatmapReportOption"),
    tags$hr(),
    uiOutput("renderExpressionReportOption")
  )
})

# Click the generate report button, render report ----
observeEvent(input$generateReport, {
  progressSweetAlert(
    session = session,
    id = "report",
    title = "Setting parameters...",
    display_pct = TRUE,
    value = 0
  )
  
  src <- normalizePath('Plot_Report.Rmd')
  
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  file.copy(src, 'Plot_Report.Rmd', overwrite = TRUE)
  
  library(rmarkdown)
  updateProgressBar(
    session = session,
    id = "report",
    title = "Start generating....",
    value = 20
  )
  
  reportParameter <- list(
    CountData = variables$CountData,
    groupList = variables$groupList,
    groupListConvert = variables$groupListConvert,
    result = variables$result,
    norData = variables$norData,
    filterLowCount = input$filterLowCount,
    normMethod = input$normMethod,
    testMethod = input$testMethod,
    iteration = input$iteration,
    fdr = input$fdr,
    floorpdeg = input$floorpdeg,
    zeroValue = variables$zeroValue,
    sampleDistributionBar = variables$sampleDistributionBar,
    sampleDistributionDensity = variables$sampleDistributionDensity,
    norSampleDistributionBar = variables$norSampleDistributionBar,
    norSampleDistributionDensity = variables$norSampleDistributionDensity,
    MAPlotObject = variables$MAPlotObject,
    VolcanoPlotObject = variables$VolcanoPlotObject,
    
    mdsPlot = NULL,
    mdsPlotplot = NULL,
    
    pcaParameter = variables$pcaParameter,
    screePlot = NULL,
    pca3d = NULL,
    pca2d = NULL,
    summaryPCA = NULL,
    
    heatmapObject = variables$heatmapObject,
    expressionLevelBar = variables$expressionLevelBar,
    expressionLevelBox = variables$expressionLevelBox,
    
    tccObject = variables$tccObject
  )
  
  updateProgressBar(
    session = session,
    id = "report",
    title = "Checking report option...",
    value = 30
  )
  
  # Check MDS
  if("MDS Plot" %in% input$importReportOption){
    reportParameter$mdsPlot <- variables$mdsPlot
    reportParameter$mdsPlotplot <- variables$mdsPlotplot
  }
  
  # Check PCA
  if("PCA Summary Table" %in% input$importReportOption){
    reportParameter$summaryPCA <- variables$summaryPCA
  }
  if("PCA Scree Plot" %in% input$importReportOption){
    reportParameter$screePlot <- variables$screePlot
  }
  if("PCA 3D Plot" %in% input$importReportOption){
    reportParameter$pca3d <- variables$pca3d
  }
  if("PCA 2D Plot" %in% input$importReportOption){
    reportParameter$pca2d <- variables$pca2d
  }
  
  updateProgressBar(
    session = session,
    id = "report",
    title = "Rendering report....",
    value = 50
  )
  out <- render('Plot_Report.Rmd', params = reportParameter, switch(
    input$format,
    Markdown = md_document(),
    HTML = html_document(),
    Word = word_document()
  ))
  variables$reportFile <- out
  runReport$runReportValue <- input$generateReport
  
  for(i in seq(51, 100, 3)){
    updateProgressBar(
      session = session,
      id = "report",
      title = "Saving report....",
      value = i 
    )
  }
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "Click [Download] to save your report.",
                 type = "success")
})

output$renderDownloadButton <- renderUI({
  if (runReport$runReportValue) {
    tagList(tags$br(), downloadButton('downloadPlotReport'))
  } else {
    helpText("Click [Generate Report] for generation.")
  }
})

output$downloadPlotReport <- downloadHandler(
  filename = function() {
    paste('Plot_Report', sep = '.', switch(
      input$format,
      Markdown = 'md',
      HTML = 'html',
      Word = 'docx'
    ))
  },

  content = function(file) {
    file.rename(variables$reportFile, file)
  }
)



# Tab click logs -----
observeEvent(input$sider, {
  clickTab <- switch(
    input$sider,
    "welcome" = "Guidance",
    "dateImport" = "Data Import",
    "calculationTab" = "Calculation",
    "maplotTab" = "MA Plot",
    "volcanoplotTab" = "Volcano Plot",
    "pcaTab" = "PCA Analysis",
    "heatmapTab" = "Heatmap",
    "expressionTab" = "Expression",
    "reportTab" = "Report"
  )
  variables$logList <-
    rbind(variables$logList,
          list(
            "Time" = as.character(Sys.time()),
            "Type" = "Tab",
            "Action" = paste0(clickTab, collapse = "-"),
            "Parameters" = ""
          ),
          stringsAsFactors = FALSE)
})


# Load Sample Data botton log -----
observeEvent(input$CountDataSample, {
  variables$logList <- rbind(
    variables$logList,
    list(
      "Time" = as.character(Sys.time()),
      "Type" = "Button",
      "Action" = "Load sample data",
      "Parameters" = input$SampleDatabase
    ),
    stringsAsFactors = FALSE
  )
})

# Click TCC botton log ----
observeEvent(input$TCC, {
  TCCParaLog <- paste(
    "Filter low count genes threshold:",
    input$filterLowCount,
    "Normalization method:",
    input$normMethod,
    "DEGs identify method:",
    input$testMethod,
    "Interation:",
    input$iteration,
    "FDR:",
    input$fdr,
    "Elimination of Potential DEGs:",
    input$floorpdeg,
    sep = " "
  )
  variables$logList <- rbind(
    variables$logList,
    list(
      "Time" = as.character(Sys.time()),
      "Type" = "Button",
      "Action" = "Run TCC",
      "Parameters" = TCCParaLog
    ),
    stringsAsFactors = FALSE
  )
})


# Click MA botton log ----
observeEvent(input$makeMAPlot , {
  MAParaLog <- paste(
    "Point Size:",
    input$pointSize,
    "FDR:",
    input$maFDR,
    "DEGs color:",
    input$fdrColor,
    sep = " "
  )
  variables$logList <- rbind(
    variables$logList,
    list(
      "Time" = as.character(Sys.time()),
      "Type" = "Button",
      "Action" = "Generate MA-Plot",
      "Parameters" = MAParaLog
    ),
    stringsAsFactors = FALSE
  )
})

# Click Volcano botton log ----

observeEvent(input$makeVolcanoPlot , {
  VolcanoParaLog <- paste(
    "Fold Change cut-off:",
    paste(input$CutFC, collapse = "~"),
    "p-value cut-off:",
    input$Cutpvalue,
    "Point Size:",
    input$pointSize,
    "Down-regulate:",
    input$downColor,
    "Up-regulate:",
    input$upColor,
    sep = " "
  )
  variables$logList <- rbind(
    variables$logList,
    list(
      "Time" = as.character(Sys.time()),
      "Type" = "Button",
      "Action" = "Generate Volcano Plot",
      "Parameters" = VolcanoParaLog
    ),
    stringsAsFactors = FALSE
  )
})


# Click PCA botton log ----

observeEvent(input$pcRun, {
  if(input$pcFDR != ""){
    pcaFDR <- paste0("FDR:", input$pcFDR, sep = " ")
  } else {
    pcaFDR <- ""
  }
  pcaParaLog <- paste(
    pcaFDR,
    "Center:",
    input$pcCenter,
    "Scale:",
    input$pcScale,
    "Log transform:",
    input$pcTransform,
    "Source:",
    input$pcData,
    "Hierarchical Clustering Method:",
    input$dendMethod,
    sep = " "
  )
  variables$logList <- rbind(
    variables$logList,
    list(
      "Time" = as.character(Sys.time()),
      "Type" = "Button",
      "Action" = "Run PCA Analysis",
      "Parameters" = pcaParaLog
    ),
    stringsAsFactors = FALSE
  )
})

# Input table ----
output$inputLogTable <- DT::renderDataTable({
  DT::datatable(variables$logList)
})

# AllInputs <- reactive({
#   x <- reactiveValuesToList(input)
#   data.frame(
#     names = names(x),
#     values = unlist(x, use.names = FALSE)
#   )
# })
# 
# output$showInputs <- renderTable({
#   AllInputs()
# })