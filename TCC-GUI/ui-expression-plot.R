# ui-expression-plot.R

fluidPage(column(
  3,
  box(
    title = tagList(icon("gears"), "Expression Level Parameters"),
    solidHeader = TRUE,
    width = NULL,
    status = "primary",
    uiOutput("expressionParameters")
  ),
  box(
    title = tagList(icon("code"), "Expression Level R Code"),
    solidHeader = TRUE,
    width = NULL,
    status = "danger",
    collapsible = TRUE,
    collapsed = TRUE,
    tabsetPanel(
      tabPanel(title = tagList(icon("chart-column"), "Barplot"),
               verbatimTextOutput("expressionLevelCodeText")),
      tabPanel(title = tagList(icon("chart-line"), "Boxplot"),
               verbatimTextOutput("expressionLevelBoxCodeText"))
    )
  )
),
column(
  9,
  tabBox(
    title = "Expression Level",
    width = NULL,
    tabPanel(title = tagList(icon("chart-column"), "Barplot"),
             uiOutput("geneBarPlotUI")),
    tabPanel(title = tagList(icon("chart-line"), "Boxplot"),
             uiOutput("geneBoxPlotUI"))
  ),
  tabBox(
    title = "Table of Expression Level",
    width = NULL,
    tabPanel(title = tagList(icon("table"), "Expression Table"),
             DT::dataTableOutput("geneTable")),
    tabPanel(title = tagList(icon("table-columns"), "Result Table"),
             DT::dataTableOutput("geneTableCal"))
  )
))
