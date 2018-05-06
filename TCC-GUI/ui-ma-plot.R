# ui-ma-plot.R

fluidRow(
  column(3, 
         wellPanel(
           tags$h4("MA-Plot"),
           tags$hr(),
           uiOutput("MAPlotParameter"),
           DT::dataTableOutput("fdrCutoffTableInMAPage")
         )
  ),#column
  column(6,
         tags$hr(),
         tags$h3("MA Plot"),
         plotlyOutput("maploty"),
         tags$h3("Result Table"),
         DT::dataTableOutput('resultTableInPlot')
  ),#column
  column(3,
         tags$hr(),
         wellPanel(
           tags$h4("Other"),
           tags$hr()
         )
  )
)