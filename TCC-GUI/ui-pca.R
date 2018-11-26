# ui-pca.R

fluidPage(column(
  3,
  box(
    title = "PCA Parameters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("pcaParameter")
  ),
  box(title = "PCA Code",
      solidHeader = TRUE,
      status = "danger",
      collapsible = TRUE,
      collapsed = TRUE,
      width = NULL,
      verbatimTextOutput("runPCACode")
  )
),
column(
  9,
  tabBox(
    title = "PCA Plot",
    width = NULL,
    tabPanel("Scree Plot",
             plotlyOutput("pcaVariances")),
    tabPanel("PCA Plot (3D)",
             plotlyOutput("pcabiplot3d")),
    tabPanel("PCA Plot (2D)",
             plotlyOutput("pcabiplot"))#,
    # tabPanel("Cluster",
    #          plotOutput("pcacluster")),
    # )
  ),
  box(
    title = "Summary of PCA",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    DT::dataTableOutput("summaryPCA")
  )
))