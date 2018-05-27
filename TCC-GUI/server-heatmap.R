# server-heatmap.R

observeEvent(input$TCC, {
  output$heatmapParameter <- renderUI({
    tagList(
      if(input$testMethod != 'wad') {
        sliderInput("heatmapFDR", "FDR:", min = 0.01, max = 1, value = 0.05)
      },
      numericInput("heatmapFDRTop", "Top gene count:", value = 50),
      radioButtons("heatmapData", "Source:", 
                   choices = c("Original" = "o",
                               "Normalized" = "n")),
      selectInput("heatmapDist", 
                  "Distance Method",
                  choices = list("Euclidean" = "euclidean",
                                 "Maximum" = "maximum",
                                 "Manhattan" = "manhattan",
                                 "Canberra" = "canberra",
                                 "Binary" = "binary",
                                 "Minkowski" = "minkowski"),
                  selected = "euclidean"),
      selectInput("heatmapCluster", 
                  "Hierarchical Clustering Method",
                  choices = list("ward.D" = "ward.D",
                                 "ward.D2" = "ward.D2",
                                 "Single" = "single",
                                 "Complete" = "complete",
                                 "UPGMA" = "average",
                                 "WPGMA" = "mcquitty",
                                 "WOGMC" = "median",
                                 "UPGMC" = "centroid"),
                  selected = "complete"),
      selectInput("heatmapScale",
                  "Scale",
                  choices = list("Row" = "column",
                                 "Column" = "row",
                                 "None" = "none")),
      actionButton("heatmapRun", "Run")
    )
  })
})

observeEvent(input$heatmapRun, {
  req(input$heatmapRun)
  isolate({
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
      data <- data[resultTable()$rank <= input$heatmapFDRTop, ]
    } else {
      data <- data[resultTable()$q.value <= input$heatmapFDR & resultTable()$rank <= input$heatmapFDRTop, ]
    }
    showNotification(paste0(dim(data)[1], " DEGs, ", dim(data)[2], " sample will be used."))
    showNotification("Generating, please be patient...", type = "message")
    
    output$heatmap <- renderPlotly({
      heatmaply(t(data), 
                k_row = length(variables$groupList),
                colors = RdYlGn,
                dist_method = input$heatmapDist,
                hclust_method = input$heatmapCluster,
                xlab = "Gene",
                ylab = "Sample",
                main = paste0("Heatmap of gene expression (FDR < ", input$heatmapFDR, ", ", dim(data)[1], "DEGs)"),
                margins = c(150,100,40,20),
                scale = input$heatmapScale,
                labCol = colnames(t(data)),
                labRow = row.names(t(data))
                )
    })
  })
})