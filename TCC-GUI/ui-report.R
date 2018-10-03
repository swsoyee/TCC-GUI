# ui-report.R

fluidPage(column(
  3,
  box(
    title = "Report Parameters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    radioButtons(
      'format',
      'Document format',
      c('Markdown', 'HTML', 'Word'),
      inline = TRUE
    ),
    downloadButton('downloadReport')
  )
))