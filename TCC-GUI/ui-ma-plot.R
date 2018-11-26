# ui-ma-plot.R

fluidPage(fluidRow(column(
  3,
  box(
    title = "MA Plot Parameters",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("MAPlotParameter")
  )
),
column(
  6,
  box(
    title = "MA Plot",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    withBarsUI(plotlyOutput("maploty"))
  )
),
column(
  3,
  box(
    title = "Expression Level",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    withBarsUI(plotlyOutput("geneBarPlot"))
  )
)),
fluidRow(column(
  3,
  tabBox(
    title = "FDR vs DEGs",
    width = NULL,
    tabPanel(
      "Table",
      tags$p("Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off."),
      DT::dataTableOutput("fdrCutoffTableInMAPage")
    ),
    tabPanel(
      "Plot",
      tags$p("Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off."),
      withBarsUI(plotlyOutput("fdrCutoffPlotInMAPage"))
    )
  )
),
column(
  6,
  box(
    title = "Result Table",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    tagList(DT::dataTableOutput('resultTableInPlot'))
  )
),
column(
  3,
  box(
    title = "MA Plot Code",
    solidHeader = TRUE,
    status = "danger",
    collapsible = TRUE,
    collapsed = TRUE,
    width = NULL,
    verbatimTextOutput("runMAPlot")
  )
)))
