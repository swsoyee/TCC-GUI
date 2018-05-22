# ui-homepage.R

fluidRow(
  column(3, 
         wellPanel(
           tags$h4("Pipeline of TCC-GUI"),
           tags$a("1.  Data input", href = "#1. Data input"),
           tags$br(),
           tags$a("2.  Computation", href = "#2. Computation"),
           tags$br(),
           tags$a("3.1 MA plot", href = "#3.1 MA plot"),
           tags$br(),
           tags$a("3.2 Volcano plot", href = "#3.2 Volcano plot"),
           tags$br(),
           tags$a("3.3 PCA analysis", href = "#3.3 PCA analysis"),
           tags$br(),
           tags$a("3.4 Heatmap", href = "#3.4 Heatmap"),
           tags$br(),
           tags$a("4. More helps", href = "#4. More helps")
           )
         ), #column
column(9,
       includeMarkdown("document/README_Japanese.md")
       )
)