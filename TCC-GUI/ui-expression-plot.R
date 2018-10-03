# ui-expression-plot.R

fluidPage(column(
  3,
  box(
    title = "Expression Parameters",
    solidHeader = TRUE,
    width = NULL,
    status = "primary",
    uiOutput("expressionParameters")
  )
),
column(
  9,
  tabBox(
    title = "Expression Level",
    width = NULL,
    tabPanel(title = "Barplot",
             plotlyOutput("geneBarPlotExpression")),
    tabPanel(title = "Boxplot",
             plotlyOutput("geneBoxPlotExpression"))
  ),
  tabBox(
    title = "Table of Expression Level",
    width = NULL,
    tabPanel(title = "Expression Table",
             DT::dataTableOutput("geneTable")),
    tabPanel(title = "Result Table",
             DT::dataTableOutput("geneTableCal"))
  )
))
