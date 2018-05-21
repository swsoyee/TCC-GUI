# server-pca.R

observeEvent(input$TCC, {
  output$pcaParameter <- renderUI({
    tagList(
      if(input$testMethod != 'wad') {
        sliderInput("pcFDR", "FDR", min = 0.01, max = 1, value = 0.05)
      },
      # numericInput("pcNumber", 
      #              "Number of Principal Components:", 
      #              10, 
      #              min = 1, 
      #              max = nrow(variables$CountData)
      #              ),
      checkboxInput("pcCenter", "Center", value = TRUE),
      checkboxInput("pcScale", "Scale", value = TRUE),
      checkboxInput("pcTransform", "Log transform", value = TRUE),
      radioButtons("pcData", "Source:", 
                   choices = c("Original" = "o",
                               "Normalized" = "n")),
      selectInput("dendMethod", 
                  "Hierarchical Clustering Method",
                  choices = list("ward.D" = "ward.D",
                                 "ward.D2" = "ward.D2",
                                 "Single" = "single",
                                 "Complete" = "complete",
                                 "UPGMA" = "average",
                                 "WPGMA" = "mcquitty",
                                 "WOGMC" = "median",
                                 "UPGMC" = "centroid")),
      actionButton("pcRun", "Run")
    )
  })
})

observeEvent(input$pcRun, {
  # Select Sample (Column)
  # Grouping.
  data.cl <- rep(0, ncol(variables$CountData))

  for (i in 1:length(variables$groupList)) {
    data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = variables$CountData))] = i
  }
  # Using Original Dataset or Normalized Dataset.
  if (input$pcData == "o") {
    data <- variables$CountData[data.cl != 0]
  } else {
    data <- variables$norData
  }
  data.cl <- data.cl[data.cl != 0]

  # Select DEGs (Row)
  if(input$testMethod == 'wad') {
    data <- data
  } else {
    data <- data[resultTable()$q.value <= input$pcFDR, ]
  }
  showNotification(paste0(dim(data)[1], " DEGs, ", dim(data)[2], " sample will be used."))
  
  # PCA processing
  # 
  if (input$pcTransform == TRUE) {
    data <- t(log(data + 0.001))
  } else {
    data <- t(data)
  }
  data.pca <- prcomp(data,
                     center = input$pcCenter,
                     scale. = input$pcScale) 
  
  # Scree Plot
  summaryTable <- summary(data.pca)$importance
  
  output$pcaVariances <-renderPlotly(
    plot_ly(x = colnames(summaryTable), 
            y = summaryTable[2, ], 
            text = paste0(summaryTable[2, ] * 100, "%"), 
            textposition = "auto",
            type = "bar",
            name = "Proportion of Variance") %>%
      add_trace(y = summaryTable[3, ],
                type = "scatter",
                mode = "lines+markers",
                name = "Cumulative Proportion") %>%
      layout(xaxis = list(title = "Principal Components"),
             yaxis = list(title = "Proportion"),
             title = "Scree Plot",
             legend = list(orientation = 'h'))
      
  )
  
  # Scatter Plot 2D
  output$pcabiplot <- renderPlotly(
    plot_ly(data = data.frame(data.pca$x),
            x = ~PC1,
            y = ~PC2,
            color = as.factor(data.cl),
            text = row.names(data.pca$x),
            textposition = "top right",
            type = "scatter",
            mode = "markers+text") %>%
      layout(title = "PCA Clusters Plot",
             legend = list(orientation = 'h'))
  )
  
  # Scatter Plot 3D
  output$pcabiplot3d <- renderPlotly(
    plot_ly(data = data.frame(data.pca$x),
            x = ~PC1,
            y = ~PC2,
            z = ~PC3,
            color = as.factor(data.cl),
            text = row.names(data.pca$x),
            textposition = "top right",
            type = "scatter3d",
            mode = "markers+text") %>%
      layout(title = "PCA Clusters Plot 3D",
             legend = list(orientation = 'h'))
  )
  
  # Cluster Dendrogram
  data.cluster <- hclust(dist(data.pca$x),method = input$dendMethod)
  
  output$pcacluster <- renderPlot(
    plot(data.cluster, xlab = "Sample")
  )
})