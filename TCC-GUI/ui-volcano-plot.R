# ui-volcano-plot.R
fluidPage(fluidRow(column(
  3,
  box(
    title = "Volcano Plot Parameters",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("valcanoParameter")
  ),
  tabBox(
    title = "FDR vs DEGs",
    width = NULL,
    tabPanel(
      title = "Table",
      tags$p("Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off."),
      DT::dataTableOutput("fdrCutoffTableInVolcano")
    ),
    tabPanel(
      title = "Plot",
      tags$p("Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off."),
      withBarsUI(plotlyOutput("fdrCutoffPlotInVolcano"))
    )
  )
),
#column
column(
  6,
  box(
    title = "Volcano Plot",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    withBarsUI(plotlyOutput("volcanoPloty"))
  ),
  box(
    title = "Result Table",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    DT::dataTableOutput('resultTableInVolcanalPlot')
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
    withBarsUI(plotlyOutput("geneBarPlotInVolcano"))
  ),
  box(title = "Volcano Plot Code",
      solidHeader = TRUE,
      status = "danger",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      verbatimTextOutput("runVolcanoPlot")
  )
)))