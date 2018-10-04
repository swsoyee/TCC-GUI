# ui-pca.R

fluidPage(column(
  3,
  box(
    title = "PCA Parameters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("pcaParameter")
  )
),
column(
  9,
  tabBox(
    title = "PCA Analysis",
    width = NULL,
    tabPanel("3D",
             plotlyOutput("pcabiplot3d")),
    tabPanel("2D",
             plotlyOutput("pcabiplot")),
    tabPanel("Cluster",
             plotOutput("pcacluster")),
    tabPanel("Variances",
             plotlyOutput("pcaVariances"))
  ),
  box(
    title = "Summary of PCA",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    DT::dataTableOutput("summaryPCA")
  )
))