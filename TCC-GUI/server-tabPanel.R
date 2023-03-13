# server-tabPanel.R
# Show calculation tab when group info has been confirmed -----------------


observeEvent(input$confirmedGroupList, {
  output$calculationTab <- renderMenu({
    menuItem(
      "TCC Computation",
      tabName = "calculationTab",
      icon = icon("calculator"),
      badgeLabel = "Step 2",
      badgeColor = "red"
    )
  })
})


# Show all tabs when calculation finished ---------------------------------


observeEvent(input$TCC, {
  output$maplotTab <- renderMenu({
    menuItem(
      "MA Plot",
      tabName = "maplotTab",
      icon = icon("chart-line"),
      badgeLabel = "Step 3",
      badgeColor = "maroon"
    )
  })
  output$volcanoplotTab <- renderMenu({
    menuItem(
      "Volcano Plot",
      tabName = "volcanoplotTab",
      icon = icon("chart-area"),
      badgeLabel = "Step 3",
      badgeColor = "maroon"
    )
  })
  # output$pcaTab <- renderMenu({
  #   menuItem(
  #     "PCA",
  #     tabName = "pcaTab",
  #     icon = icon("tachometer"),
  #     badgeLabel = "Step 3",
  #     badgeColor = "maroon"
  #   )
  # })
  output$heatmapTab <- renderMenu({
    menuItem(
      "Heatmap",
      tabName = "heatmapTab",
      icon = icon("table-cells"),
      badgeLabel = "Step 3",
      badgeColor = "maroon"
    )
  })
  output$expressionTab <- renderMenu({
    menuItem(
      "Expression Level",
      tabName = "expressionTab",
      icon = icon("chart-column"),
      badgeLabel = "Step 3",
      badgeColor = "maroon"
    )
  })
  output$reportTab <- renderMenu({
    menuItem(
      "Report",
      tabName = "reportTab",
      icon = icon("file-pdf"),
      badgeLabel = "Step 4",
      badgeColor = "fuchsia"
    )
  })
})