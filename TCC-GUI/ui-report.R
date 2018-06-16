# ui-report.R

fluidPage(column(3,
                 tags$hr(),
                 wellPanel(
                   tags$h4("Report Parameters"),
                   tags$hr(),
                   radioButtons('format', 'Document format', c('Markdown', 'HTML', 'Word'),
                                inline = TRUE),
                   downloadButton('downloadReport')
                 )))