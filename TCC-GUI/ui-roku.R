# ui-roku.R

fluidPage(
  column(3,
         tags$hr(),
         wellPanel(
           tags$h4("ROKU"),
           tags$hr(),
           actionButton("runROKU", "Run")
         )),
  column(
    9,
    tags$hr() # ,
    # withBarsUI(plotlyOutput("heatmap")),
  )
)