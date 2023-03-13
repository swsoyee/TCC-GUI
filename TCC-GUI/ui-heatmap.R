# ui-heatmap.R

fluidPage(column(
  3,
  box(
    title = tagList(icon("gears"), "Heatmap Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("heatmapParameter")
  ),
  box(
    title = tagList(icon("code"), "Heatmap R Code"),
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
    title = tagList(icon("table-cells"), "Heatmap"),
    width = NULL,
    solidHeader = TRUE,
    status = "info", 
    footer = "It will be very time consuming if the number of genes is over hundred. Reduce the number by cutoff or wait patiently.",
    uiOutput("heatmapPlot")
  ),
  box(
    title = tagList(icon("table"), "Listed Gene Information Table"),
    width = TRUE,
    solidHeader = TRUE,
    status = "info",
    DT::dataTableOutput("resultTableInHeatmap")
  )
))