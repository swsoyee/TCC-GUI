# ui-heatmap.R

fluidPage(column(
  3,
  box(
    title = "Heatmap Parameters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("heatmapParameter")
  )
),
column(
  9,
  box(
    title = "Heatmap",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    withBarsUI(plotlyOutput("heatmap")),
    DT::dataTableOutput("resultTableInHeatmap")
  )
))