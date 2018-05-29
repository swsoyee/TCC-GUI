# ui-pca.R

fluidPage(column(3,
                 tags$hr(),
                 wellPanel(
                   tags$h4("PCA Parameters"),
                   tags$hr(),
                   uiOutput("pcaParameter")
                 )),
          column(
            9,
            tags$hr(),
            tabsetPanel(
              id = "pcaPlotTab",
              tabPanel("3D",
                       plotlyOutput("pcabiplot3d")),
              tabPanel("2D",
                       plotlyOutput("pcabiplot")),
              tabPanel("Cluster",
                       plotOutput("pcacluster")),
              tabPanel("Variances",
                       plotlyOutput("pcaVariances"))
            )
          ))