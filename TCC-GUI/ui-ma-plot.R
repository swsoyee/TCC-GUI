# ui-ma-plot.R

fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("gears"), "MA Plot Parameters"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("MAPlotParameter")
  ),
  tabBox(
    title = "FDR vs DEGs",
    width = NULL,
    tabPanel(
      tagList(icon("table"), "Table"),
      DT::dataTableOutput("fdrCutoffTableInMAPage")
    ),
    tabPanel(
      tagList(icon("chart-column"), "Plot"),
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
    title = tagList(icon("chart-line"), "MA Plot"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    uiOutput("MAPlotUI")
  ),
  box(
    title = tagList(icon("table"), "Result Table"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    tagList(DT::dataTableOutput('resultTableInPlot'))
  )
)))