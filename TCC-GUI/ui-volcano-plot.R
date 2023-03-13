# ui-volcano-plot.R
fluidPage(fluidRow(
  column(
    3,
    box(
      title = tagList(icon("gears"), "Volcano Plot Parameters"),
      solidHeader = TRUE,
      status = "primary",
      width = NULL,
      uiOutput("valcanoParameter")
    ),
    box(
      title = tagList(icon("code"), "Volcano Plot Code"),
      solidHeader = TRUE,
      status = "danger",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      verbatimTextOutput("runVolcanoPlot")
    )
  ),
  column(
    9,
    box(
      title = tagList(icon("chart-line"), "Volcano Plot"),
      solidHeader = TRUE,
      status = "info",
      width = NULL,
      uiOutput("volcanoUI")
    ),
    box(
      title = tagList(icon("table"), "Result Table"),
      solidHeader = TRUE,
      status = "info",
      width = NULL,
      DT::dataTableOutput("resultTableInVolcanalPlot"),
      downloadButton("download_all_volcano","Download all data as csv")
    )
  )
))
