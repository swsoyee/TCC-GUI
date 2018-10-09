# ui-ma-plot.R
fluidPage(fluidRow(column(
  3,
  box(
    title = "MA-Plot Parameters",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("MAPlotParameter")
  ),
  tabBox(
    title = "FDR vs DEGs",
    width = NULL,
    tabPanel(
      "Table",
      tags$p("DEGs count under different FDR cut-off."),
      DT::dataTableOutput("fdrCutoffTableInMAPage")
    ),
    tabPanel(
      "Plot",
      tags$p("DEGs count under different FDR cut-off."),
      withBarsUI(plotlyOutput("fdrCutoffPlotInMAPage"))
    )
  )
),
#column
column(
  6,
  box(
    title = "MA Plot",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    withBarsUI(plotlyOutput("maploty"))
  ),
  box(
    title = "Result Table",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    tagList(DT::dataTableOutput('resultTableInPlot'))
  )
),
#column
column(
  3,
  box(
    title = "Expression Level",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    withBarsUI(plotlyOutput("geneBarPlot"))
  ),
  box(title = "MA Plot Code",
      solidHeader = TRUE,
      status = "danger",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      verbatimTextOutput("runMAPlot")
  )
)))