# ui-volcano-plot.R

fluidRow(
  column(3, 
         wellPanel(
           tags$h4("Volcano Plot Parameter"),
           tags$hr(),
           uiOutput("valcanoParameter")
         )
  ),#column
  column(6,
         tags$hr(),
         tags$h3("Volcano Plot"),
         plotlyOutput("volcanoPloty"),
         tags$h3("Result Table")
         # DT::dataTableOutput('resultTableInPlot')
  ),#column
  column(3,
         tags$hr()
         # wellPanel(
         #   plotlyOutput("geneBarPlot")
         # )
  )
)