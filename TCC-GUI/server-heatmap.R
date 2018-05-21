# server-heatmap.R

observeEvent(input$TCC, {
  output$heatmapParameter <- renderUI({
    tagList(
      if(input$testMethod != 'wad') {
        sliderInput("heatmapFDR", "FDR", min = 0.01, max = 1, value = 0.05)
      },
      radioButtons("heatmapData", "Source:", 
                   choices = c("Original" = "o",
                               "Normalized" = "n")),
      # sliderInput("heatmapColorLimit", "Limitation of color range:",
      #             min = -15, max = 15, value = c(-10, 10)),
      actionButton("heatmapRun", "Run")
    )
  })
})

observeEvent(input$heatmapRun, {
  # Select Sample (Column)
  # Grouping.
  data.cl <- rep(0, ncol(variables$CountData))
  
  for (i in 1:length(variables$groupList)) {
    data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = variables$CountData))] = i
  }
  # Using Original Dataset or Normalized Dataset.
  if (input$heatmapData == "o") {
    data <- variables$CountData[data.cl != 0]
  } else {
    data <- variables$norData
  }
  data.cl <- data.cl[data.cl != 0]
  
  # Select DEGs (Row)
  if(input$testMethod == 'wad') {
    data <- data
  } else {
    data <- data[resultTable()$q.value <= input$heatmapFDR, ]
  }
  showNotification(paste0(dim(data)[1], " DEGs, ", dim(data)[2], " sample will be used."))
  showNotification("Generating, please be patient...", type = "message")
  
  output$heatmap <- renderPlotly({
    heatmaply(t(log(data + 0.01)), 
              k_row = length(variables$groupList),
              colors = RdYlGn,
              xlab = "Gene",
              ylab = "Sample",
              main = paste0("Heatmap of gene expression (FDR < ", input$heatmapFDR, ", ", dim(data)[1], "DEGs)"),
              margins = c(60,100,40,20),
              # limits = input$heatmapColorLimit
              )
  })
})