# ui-homepage.R

fluidPage(fluidRow(
  column(
    3,
    tags$hr(),
    # fixedPanel(
    wellPanel(
      style = "position:fixed;width:21.5%;",
      tags$h4("Pipeline of TCC-GUI"),
      tags$a("1.  Data input", href = "#Datainput"),
      tags$br(),
      tags$a("2.  Computation", href = "#Computation"),
      tags$hr(),
      tags$a("3.1 MA plot", href = "#MAplot"),
      tags$br(),
      tags$a("3.2 Volcano plot", href = "#Volcanoplot"),
      tags$br(),
      tags$a("3.3 PCA analysis", href = "#PCAanalysis"),
      tags$br(),
      tags$a("3.4 Heatmap", href = "#Heatmap"),
      tags$br(),
      tags$a("3.5 Expression", href = "#Expression"),
      tags$hr(),
      tags$a("4. More helps", href = "#Morehelps")
    )
  ),
  #column
  column(9,
         tags$hr(),
         tabsetPanel(
           id = "home",
           tabPanel(title = "English", includeMarkdown("document/README_English.md"))
         ))
))
