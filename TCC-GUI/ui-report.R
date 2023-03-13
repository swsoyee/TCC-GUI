# ui-report.R

fluidPage(column(
  3,
  box(
    title = tagList(icon("gears"), "Report Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    footer = "HTML report (default) is highly recommended.",
    radioGroupButtons(
      inputId = 'format',
      label = 'Document Format',
      choices = c(#'Markdown', 
                  'HTML', 
                  'Word'),
      justified = TRUE,
      status = "primary"
    ),
    do.call(actionBttn, c(
      list(
        inputId = "generateReport",
        label = "Generate Report",
        icon = icon("play")
      ),
      actionBttnParams
    )),
    
    uiOutput("renderDownloadButton")
  )
),
column(9,
       box(
         title = "Output Option",
         status = "primary",
         solidHeader = TRUE,
         width = NULL,
         uiOutput("reportOption")
       )))