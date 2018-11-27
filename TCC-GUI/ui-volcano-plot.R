# ui-volcano-plot.R
fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Volcano Plot Parameters"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("valcanoParameter")
  ),
  tabBox(
    title = "FDR vs DEGs",
    width = NULL,
    tabPanel(
      title = tagList(icon("table"), "Table"),
      tags$p("Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off."),
      DT::dataTableOutput("fdrCutoffTableInVolcano")
    ),
    tabPanel(
      title = tagList(icon("bar-chart"), "Plot"),
      tags$p("Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off."),
      withBarsUI(plotlyOutput("fdrCutoffPlotInVolcano"))
    )
  )
),
#column
column(
  6,
  box(
    title = tagList(icon("line-chart"), "Volcano Plot"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    withBarsUI(plotlyOutput("volcanoPloty"))
  ),
  box(
    title = tagList(icon("table"), "Result Table"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    DT::dataTableOutput('resultTableInVolcanalPlot')
  )
),
#column
column(
  3,
  box(
    title = tagList(icon("bar-chart"), "Expression Level"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    withBarsUI(plotlyOutput("geneBarPlotInVolcano"))
  ),
  box(title = tagList(icon("code"), "Volcano Plot Code"),
      solidHeader = TRUE,
      status = "danger",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      verbatimTextOutput("runVolcanoPlot")
  )
)))