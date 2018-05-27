# ui-heatmap.R

fluidPage(
  column(3,
         wellPanel(
           tags$h4("Heatmap Parameters"),
           tags$hr(),
           uiOutput("heatmapParameter")
           )
         ),
  column(9,
         tags$hr(),
         plotlyOutput("heatmap")
         )
)
