# ui-ma-plot.R

fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "MA Plot Parameters"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("MAPlotParameter")
  )
),
column(
  9,
  box(
    title = tagList(icon("line-chart"), "MA Plot"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    uiOutput("MAPlotUI")
  )
)),
fluidRow(column(
  3,
  tabBox(
    title = "FDR vs DEGs",
    width = NULL,
    tabPanel(
      tagList(icon("table"), "Table"),
      DT::dataTableOutput("fdrCutoffTableInMAPage")
    ),
    tabPanel(
      tagList(icon("bar-chart"), "Plot"),
      plotlyOutput("fdrCutoffPlotInMAPage") %>% withSpinner()
    )
  ),
  box(
    title = tagList(icon("code"), "MA Plot Code"),
    solidHeader = TRUE,
    status = "danger",
    collapsible = TRUE,
    collapsed = TRUE,
    width = NULL,
    verbatimTextOutput("runMAPlot")
  )
),
column(
  9,
  box(
    title = tagList(icon("table"), "Result Table"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    tagList(DT::dataTableOutput('resultTableInPlot'))
  )
)#,
# column(
#   3,
  # box(
  #   title = tagList(icon("code"), "MA Plot Code"),
  #   solidHeader = TRUE,
  #   status = "danger",
  #   collapsible = TRUE,
  #   collapsed = TRUE,
  #   width = NULL,
  #   verbatimTextOutput("runMAPlot")
  # )
))
#)
