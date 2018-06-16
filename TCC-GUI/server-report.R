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
    
    reportParameter <- list(CountData = variables$CountData)
    
    out <- render('report.Rmd', params = reportParameter, switch(
      input$format,
      Markdown = md_document(),
      HTML = html_document(),
      Word = word_document()
    ))
    file.rename(out, file)
  }
)