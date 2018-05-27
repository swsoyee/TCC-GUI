# ui-volcano-plot.R

fluidRow(
  column(3, 
         wellPanel(
           tags$h4("Volcano Plot Parameter"),
           tags$hr(),
           uiOutput("valcanoParameter")
         ),
         wellPanel(
           tags$h4("FDR vs DEGs"),
           tags$hr(),
           tags$p("DEGs count under different FDR cutoff."),
           tabsetPanel(id = "volcanoplot", 
                       tabPanel("Table", DT::dataTableOutput("fdrCutoffTableInVolcano")),
                       tabPanel("Plot", plotlyOutput("fdrCutoffPlotInVolcano")))
         )
  ),#column
  column(6,
         tags$hr(),
         tags$h3("Volcano Plot"),
         plotlyOutput("volcanoPloty"),
         tags$h3("Result Table"),
         DT::dataTableOutput('resultTableInVolcanalPlot')
  ),#column
  column(3,
         tags$hr(),
         wellPanel(
           plotlyOutput("geneBarPlotInVolcano")#,
           # plotlyOutput("geneBoxPlotInVolcano")
         )
  )
)