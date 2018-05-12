# server-tabPanel.R
#
observeEvent(input$TCC, {
  appendTab(
    inputId = "tabs",
    tabPanel("MA Plot",
             source(file = "ui-ma-plot.R", local = TRUE, encoding = "UTF-8")$value
    )
  )
  appendTab(
    inputId = "tabs",
    tabPanel("Volcano Plot",
             source(file = "ui-volcano-plot.R", local = TRUE, encoding = "UTF-8")$value
    )
  )
})