# ui-pca.R

fluidPage(column(
  3,
  box(
    title = tagList(icon("cogs"), "PCA Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("pcaParameter")
  ),
  box(title = tagList(icon("code"), "PCA Code"),
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
    title = "",
    width = NULL,
    tabPanel(tagList(icon("bar-chart"), "Scree Plot"),
             uiOutput("screePlotUI")),
    tabPanel(tagList(icon("cube"), "PCA Plot (3D)"),
             uiOutput("pca3dPlotUI")),
    tabPanel(tagList(icon("square-o"), "PCA Plot (2D)"),
             uiOutput("pca2dPlotUI"))
  ),
  box(
    title = tagList(icon("info-circle"), "Summary of PCA"),
    width = NULL,
    solidHeader = TRUE,
    status = "info",
    uiOutput("pcaSummaryTable")
  )
))