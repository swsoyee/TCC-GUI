# ui-expression-plot.R

fluidPage(
  column(3,
         tags$hr(),
         wellPanel(
           tags$h4("Expression Parameters"),
           tags$hr(),
           selectInput(
             "expressionGene",
             "Select Gene(s):",
             choices = row.names(variables$CountData),
             multiple = TRUE
           )
         )),
  column(
    9,
    tags$hr(),
    tabsetPanel(
      id = "expressionPlotTab",
      tabPanel("Barplot",
               plotlyOutput("geneBarPlotExpression")),
      tabPanel("Boxplot",
               plotlyOutput("geneBoxPlotExpression"))
    ),
    tags$hr(),
    tabsetPanel(id = "expressionTable",
                tabPanel(
                  "Expression Table",
                  DT::dataTableOutput("geneTable")
                ),
                tabPanel(
                  "Result Table",
                  DT::dataTableOutput("geneTableCal")
                ))
  )
)