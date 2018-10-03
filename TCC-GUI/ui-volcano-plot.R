# ui-volcano-plot.R
fluidPage(fluidRow(column(
  3,
  box(
    title = "Volcano Plot Parameter",
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
      tags$p("DEGs count under different FDR cutoff."),
      DT::dataTableOutput("fdrCutoffTableInVolcano")
    ),
    tabPanel(
      title = "Plot",
      tags$p("DEGs count under different FDR cutoff."),
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
  )
)))