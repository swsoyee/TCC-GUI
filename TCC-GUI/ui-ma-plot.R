# ui-ma-plot.R
fluidPage(fluidRow(
  column(
    3,
    tags$hr(),
    wellPanel(
      tags$h4("MA-Plot Parameter"),
      tags$hr(),
      uiOutput("MAPlotParameter")
    ),
    wellPanel(
      tags$h4("FDR vs DEGs"),
      tags$hr(),
      tags$p("DEGs count under different FDR cutoff."),
      tabsetPanel(
        id = "maplot",
        tabPanel("Table", DT::dataTableOutput("fdrCutoffTableInMAPage")),
        tabPanel("Plot", plotlyOutput("fdrCutoffPlotInMAPage"))
      )
    )
  ),
  #column
  column(
    6,
    tags$hr(),
    tags$h3("MA Plot"),
    plotlyOutput("maploty"),
    tags$h3("Result Table"),
    DT::dataTableOutput('resultTableInPlot')
  ),
  #column
  column(3,
         tags$hr(),
         wellPanel(plotlyOutput("geneBarPlot")))
))