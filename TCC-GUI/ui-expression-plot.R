# ui-expression-plot.R

fluidPage(column(
  3,
  box(
    title = "Expression Parameters",
    solidHeader = TRUE,
    width = NULL,
    status = "primary",
    uiOutput("expressionParameters")
  ),
  box(
    title = "Expression Level R Code",
    solidHeader = TRUE,
    width = NULL,
    status = "danger",
    collapsible = TRUE,
    collapsed = TRUE,
    tabsetPanel(
      tabPanel(title = "Barplot",
               verbatimTextOutput("expressionLevelCodeText")),
      tabPanel(title = "Boxplot",
               verbatimTextOutput("expressionLevelBoxCodeText"))
    )
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
