# server-report.R

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