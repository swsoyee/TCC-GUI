# ui-report.R

fluidPage(column(
  3,
  box(
    title = "Report Parameters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    radioGroupButtons(
      inputId = 'format',
      label = 'Document format',
      choices = c('Markdown', 'HTML', 'Word'), 
      justified = TRUE,
      status = "primary"
    ),
    downloadButton('downloadReport')
  )
))