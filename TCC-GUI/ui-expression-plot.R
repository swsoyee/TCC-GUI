# ui-expression-plot.R

fluidPage(
  column(3,
         wellPanel(
           tags$h4("Expression Parameters"),
           tags$hr(),
           selectInput("expressionGene", "Select Gene(s):",
                       choices = row.names(variables$CountData),
                       multiple = TRUE)
         )
  ),
  column(9,
         tags$hr(),
         tabsetPanel(id = "expressionPlotTab",
                     tabPanel("Barplot",
                              plotlyOutput("geneBarPlotExpression")
                     ),
                     tabPanel("Boxplot",
                              plotlyOutput("geneBoxPlotExpression")
                     )
         )
  )
)