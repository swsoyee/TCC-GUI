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
  6,
  box(
    title = tagList(icon("line-chart"), "MA Plot"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    withBarsUI(plotlyOutput("maploty"))
  )
),
column(
  3,
  box(
    title = tagList(icon("bar-chart"), "Expression Level"),
    solidHeader = TRUE,
    status = "info",
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
      tagList(icon("table"), "Table"),
      tags$p("Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off."),
      DT::dataTableOutput("fdrCutoffTableInMAPage")
    ),
    tabPanel(
      tagList(icon("bar-chart"), "Plot"),
      tags$p("Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off."),
      withBarsUI(plotlyOutput("fdrCutoffPlotInMAPage"))
    )
  )
),
column(
  6,
  box(
    title = tagList(icon("table"), "Result Table"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    tagList(DT::dataTableOutput('resultTableInPlot'))
  )
),
column(
  3,
  box(
    title = tagList(icon("code"), "MA Plot Code"),
    solidHeader = TRUE,
    status = "danger",
    collapsible = TRUE,
    collapsed = TRUE,
    width = NULL,
    verbatimTextOutput("runMAPlot")
  )
)))
