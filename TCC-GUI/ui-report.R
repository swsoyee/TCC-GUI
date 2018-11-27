# ui-report.R

fluidPage(column(
  3,
  box(
    title = tagList(icon("cogs"), "Report Parameters"),
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
    downloadButton('downloadPlotReport')
  )
)#,
# column(9,
       # box(
       #   title = "Parameters log",
       #   width = NULL,
       #   solidHeader = TRUE,
       #   status = "info",
       #   # dataTableOutput("inputLogTable"),
       #   # collapsible = TRUE,
       #   tableOutput("showInputs") #,
       # ),
       # tabBox(title = "Output part",
       #        tabPanel(title = "Simulation Data",),
       #        tabPanel(title = "Data Import"),
       #        tabPanel(title = "TCC Computation"),
       #        tabPanel(title = "MA Plot"),
       #        tabPanel(title = "Volcano Plot"),
       #        tabPanel(title = "PCA"),
       #        tabPanel(title = "Heatmap"),
       #        tabPanel(title = "Expression Level"))
       # box(title = "Report preview",
       #     width = NULL,
       #     solidHeader = TRUE,
       #     status = "success",
       #     uiOutput("reportPreview"))
       # )
)