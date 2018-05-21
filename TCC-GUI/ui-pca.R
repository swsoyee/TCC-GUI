# ui-pca.R

fluidPage(
  column(3,
         wellPanel(
           tags$h4("PCA Parameters"),
           tags$hr(),
           uiOutput("pcaParameter")
         )
  ),
  column(9,
    fluidRow(
      tags$hr(),
      column(3,
             plotlyOutput("pcabiplot3d")),
      column(3,
             plotlyOutput("pcabiplot")),
      column(3,
             plotOutput("pcacluster"))
    ),
    fluidRow(
      column(3,
             plotlyOutput("pcaVariances"))
    )
  )
)