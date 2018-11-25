# server-report.R
# output$reportPreview <- renderUI({
#   reportParameter <- list(
#     CountData = variables$CountData,
#     groupList = variables$groupList,
#     filterLowCount = input$filterLowCount,
#     normMethod = input$normMethod,
#     testMethod = input$testMethod,
#     iteration = input$iteration,
#     fdr = input$fdr,
#     floorpdeg = input$floorpdeg,
#     runMAPlot = variables$runMAPlot,
#     resultTableInPlot_rows_selected = input$resultTableInPlot_rows_selected,
#     GeneAttribute = input$GeneAttribute,
#     maFDR = input$maFDR,
#     fdrColor = input$fdrColor,
#     runVolcanoPlot = variables$runVolcanoPlot,
#     CutFC = input$CutFC,
#     Cutpvalue = input$Cutpvalue,
#     resultTableInVolcanalPlot_rows_selected = input$resultTableInVolcanalPlot_rows_selected,
#     downColor = input$downColor,
#     upColor = input$upColor,
#     xlabs = input$xlabs,
#     ylabs = input$ylabs,
#     graphicTitle = input$graphicTitle
#   )
#   # report <- rmarkdown::render('report.Rmd', params = reportParameter, md_document())
#   includeMarkdown("report.Rmd")
# })

# output$downloadReport <- downloadHandler(
#   filename = function() {
#     paste('my-report', sep = '.', switch(
#       input$format,
#       Markdown = 'md',
#       HTML = 'html',
#       Word = 'docx'
#     ))
#   },
#   
#   content = function(file) {
#     src <- normalizePath('report.Rmd')
#     
#     owd <- setwd(tempdir())
#     on.exit(setwd(owd))
#     file.copy(src, 'report.Rmd', overwrite = TRUE)
#     
#     library(rmarkdown)
#     
#     reportParameter <- list(
#       CountData = variables$CountData,
#       groupList = variables$groupList,
#       filterLowCount = input$filterLowCount,
#       normMethod = input$normMethod,
#       testMethod = input$testMethod,
#       iteration = input$iteration,
#       fdr = input$fdr,
#       floorpdeg = input$floorpdeg,
#       runMAPlot = variables$runMAPlot,
#       resultTableInPlot_rows_selected = input$resultTableInPlot_rows_selected,
#       GeneAttribute = input$GeneAttribute,
#       maFDR = input$maFDR,
#       fdrColor = input$fdrColor,
#       runVolcanoPlot = variables$runVolcanoPlot,
#       CutFC = input$CutFC,
#       Cutpvalue = input$Cutpvalue,
#       resultTableInVolcanalPlot_rows_selected = input$resultTableInVolcanalPlot_rows_selected,
#       downColor = input$downColor,
#       upColor = input$upColor,
#       xlabs = input$xlabs,
#       ylabs = input$ylabs,
#       graphicTitle = input$graphicTitle
#     )
#     
#     out <- render('report.Rmd', params = reportParameter, switch(
#       input$format,
#       Markdown = md_document(),
#       HTML = html_document(),
#       Word = word_document()
#     ))
#     file.rename(out, file)
#   }
# )

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
    src <- normalizePath('Plot_Report.Rmd')

    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'Plot_Report.Rmd', overwrite = TRUE)

    library(rmarkdown)

    reportParameter <- list(
      CountData = variables$CountData,
      groupList = variables$groupList,
      zeroValue = variables$zeroValue,
      sampleDistributionBar = variables$sampleDistributionBar,
      sampleDistributionDensity = variables$sampleDistributionDensity,
      norSampleDistributionBar = variables$norSampleDistributionBar,
      norSampleDistributionDensity = variables$norSampleDistributionDensity,
      MAPlotObject = variables$MAPlotObject,
      VolcanoPlotObject = variables$VolcanoPlotObject,
      screePlot = variables$screePlot,
      pca3d = variables$pca3d,
      pca2d = variables$pca2d,
      heatmapObject = variables$heatmapObject,
      expressionLevelBar = variables$expressionLevelBar,
      expressionLevelBox = variables$expressionLevelBox
    )

    out <- render('Plot_Report.Rmd', params = reportParameter, switch(
      input$format,
      Markdown = md_document(),
      HTML = html_document(),
      Word = word_document()
    ))
    file.rename(out, file)
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