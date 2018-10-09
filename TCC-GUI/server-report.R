# server-report.R
output$reportPreview <- renderUI({
  reportParameter <- list(
    CountData = variables$CountData,
    groupList = variables$groupList,
    filterLowCount = input$filterLowCount,
    normMethod = input$normMethod,
    testMethod = input$testMethod,
    iteration = input$iteration,
    fdr = input$fdr,
    floorpdeg = input$floorpdeg,
    runMAPlot = variables$runMAPlot,
    resultTableInPlot_rows_selected = input$resultTableInPlot_rows_selected,
    GeneAttribute = input$GeneAttribute,
    maFDR = input$maFDR,
    fdrColor = input$fdrColor,
    runVolcanoPlot = variables$runVolcanoPlot,
    CutFC = input$CutFC,
    Cutpvalue = input$Cutpvalue,
    resultTableInVolcanalPlot_rows_selected = input$resultTableInVolcanalPlot_rows_selected,
    downColor = input$downColor,
    upColor = input$upColor,
    xlabs = input$xlabs,
    ylabs = input$ylabs,
    graphicTitle = input$graphicTitle
  )
  # report <- rmarkdown::render('report.Rmd', params = reportParameter, md_document())
  includeMarkdown("report.Rmd")
})

output$downloadReport <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format,
      Markdown = 'md',
      HTML = 'html',
      Word = 'docx'
    ))
  },
  
  content = function(file) {
    src <- normalizePath('report.Rmd')
    
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    
    reportParameter <- list(
      CountData = variables$CountData,
      groupList = variables$groupList,
      filterLowCount = input$filterLowCount,
      normMethod = input$normMethod,
      testMethod = input$testMethod,
      iteration = input$iteration,
      fdr = input$fdr,
      floorpdeg = input$floorpdeg,
      runMAPlot = variables$runMAPlot,
      resultTableInPlot_rows_selected = input$resultTableInPlot_rows_selected,
      GeneAttribute = input$GeneAttribute,
      maFDR = input$maFDR,
      fdrColor = input$fdrColor,
      runVolcanoPlot = variables$runVolcanoPlot,
      CutFC = input$CutFC,
      Cutpvalue = input$Cutpvalue,
      resultTableInVolcanalPlot_rows_selected = input$resultTableInVolcanalPlot_rows_selected,
      downColor = input$downColor,
      upColor = input$upColor,
      xlabs = input$xlabs,
      ylabs = input$ylabs,
      graphicTitle = input$graphicTitle
    )
    
    out <- render('report.Rmd', params = reportParameter, switch(
      input$format,
      Markdown = md_document(),
      HTML = html_document(),
      Word = word_document()
    ))
    file.rename(out, file)
  }
)

##################
# Tab click log
##################
observeEvent(input$sider, {
  clickTab <- switch(input$sider,
                     "dateImport" = "Click [Data Import] tab",
                     "calculationTab" = "Click [Calculation] tab",
                     "maplotTab" = "Click [MA Plot] tab",
                     "volcanoplotTab" = "Click [Volcano Plot] tab",
                     "pcaTab" = "Click [PCA Analysis] tab",
                     "heatmapTab" = "Click [Heatmap] tab",
                     "expressionTab" = "Click [Expression] tab",
                     "reportTab" = "Click [Report] tab")
  clickTab <- paste0(Sys.time(), " |", clickTab)
  variables$logList <- c(variables$logList, clickTab)
})

##################
# Load Sample Data botton log
##################
observeEvent(input$CountDataSample, {
  CountDataSample <-
    paste(Sys.time(), "|Click [Load sample data] button and load:", input$SampleDatabase)
  variables$logList <- c(variables$logList, CountDataSample)
})

##################
# Click TCC botton log
##################
observeEvent(input$TCC, {
  TCC <-
    sprintf(
      "|Click [Run TCC] button: <br/>Filter low count genes threshold: %s<br/>Normalization method: %s<br/>DEGs identify method: %s<br/>Interation: %s<br/>FDR: %s<br/>Elimination of Potential DEGs:%s<br/>",
      input$filterLowCount,
      input$normMethod,
      input$testMethod,
      input$iteration,
      input$fdr,
      input$floorpdeg
    )
  TCC <- paste(Sys.time(), TCC)
  variables$logList <- c(variables$logList, TCC)
})

observeEvent(reactiveValuesToList(input), {
  output$inputLog <-
    renderUI(HTML(paste(variables$logList, collapse = '<br/>--------<br/>')))
})