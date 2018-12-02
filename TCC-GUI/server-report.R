# server-report.R

runReport <- reactiveValues(runReportValue = FALSE)

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
    pcScale = input$pcScale,
    pcCenter = input$pcCenter,
    pcTransform = input$pcTransform,
    screePlot = variables$screePlot,
    pca3d = variables$pca3d,
    pca2d = variables$pca2d,
    heatmapObject = variables$heatmapObject,
    expressionLevelBar = variables$expressionLevelBar,
    expressionLevelBox = variables$expressionLevelBox
  )
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
    # src <- normalizePath('Plot_Report.Rmd')
    # 
    # owd <- setwd(tempdir())
    # on.exit(setwd(owd))
    # file.copy(src, 'Plot_Report.Rmd', overwrite = TRUE)
    # 
    # library(rmarkdown)
    # 
    # reportParameter <- list(
    #   CountData = variables$CountData,
    #   groupList = variables$groupList,
    #   groupListConvert = variables$groupListConvert,
    #   result = variables$result,
    #   norData = variables$norData,
    #   filterLowCount = input$filterLowCount,
    #   normMethod = input$normMethod,
    #   testMethod = input$testMethod,
    #   iteration = input$iteration,
    #   fdr = input$fdr,
    #   floorpdeg = input$floorpdeg,
    #   zeroValue = variables$zeroValue,
    #   sampleDistributionBar = variables$sampleDistributionBar,
    #   sampleDistributionDensity = variables$sampleDistributionDensity,
    #   norSampleDistributionBar = variables$norSampleDistributionBar,
    #   norSampleDistributionDensity = variables$norSampleDistributionDensity,
    #   MAPlotObject = variables$MAPlotObject,
    #   VolcanoPlotObject = variables$VolcanoPlotObject,
    #   pcScale = input$pcScale,
    #   pcCenter = input$pcCenter,
    #   pcTransform = input$pcTransform,
    #   screePlot = variables$screePlot,
    #   pca3d = variables$pca3d,
    #   pca2d = variables$pca2d,
    #   heatmapObject = variables$heatmapObject,
    #   expressionLevelBar = variables$expressionLevelBar,
    #   expressionLevelBox = variables$expressionLevelBox
    # )
    # 
    # out <- render('Plot_Report.Rmd', params = reportParameter, switch(
    #   input$format,
    #   Markdown = md_document(),
    #   HTML = html_document(),
    #   Word = word_document()
    # ))
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