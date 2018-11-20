# ui-heatmap.R

fluidPage(column(
  3,
  box(
    title = "Heatmap Parameters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("heatmapParameter")
  ),
  box(
    title = "Heatmap R Code",
    width = NULL,
    status = "danger",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    verbatimTextOutput("heatmapRcode")
  )
),
column(
  9,
  box(
    title = "Heatmap",
    width = NULL,
    solidHeader = TRUE,
    status = "info",
    uiOutput("heatmapPlot")
  ),
  box(
    title = "Gene List Table",
    width = TRUE,
    solidHeader = TRUE,
    status = "info",
    DT::dataTableOutput("resultTableInHeatmap")
  )
))