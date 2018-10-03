# server-tabPanel.R
#
observeEvent(input$confirmedGroupList, {
  output$calculationTab <- renderMenu({
    menuItem("Calculation", tabName = "calculationTab", icon = icon("calculator"))
  })
})

observeEvent(input$TCC, {
    output$maplotTab <- renderMenu({
      menuItem("MA Plot", tabName = "maplotTab", icon = icon("line-chart"))
    })
    output$volcanoplotTab <- renderMenu({
      menuItem("Volcano Plot", tabName = "volcanoplotTab", icon = icon("area-chart"))
    })
    output$pcaTab <- renderMenu({
      menuItem("PCA", tabName = "pcaTab", icon = icon("tachometer"))
    })
    output$heatmapTab <- renderMenu({
      menuItem("Heatmap", tabName = "heatmapTab", icon = icon("th"))
    })
    output$expressionTab <- renderMenu({
      menuItem("Expression", tabName = "expressionTab", icon = icon("bar-chart"))
    })
    output$reportTab <- renderMenu({
      menuItem("Report", tabName = "reportTab", icon = icon("file-pdf-o"))
    })
})