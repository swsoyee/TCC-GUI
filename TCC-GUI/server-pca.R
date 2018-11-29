# server-pca.R


observeEvent(input$sider, {
  if (input$sider == "pcaTab") {
    output$pcaParameter <- renderUI({
      tagList(
        if (input$testMethod != 'wad') {
          sliderInput("pcFDR",
                      tagList("FDR Cut-off", helpText("(Set 1 for Exploratory Analysis with all genes)")),
                      min = 0.01,
                      max = 1,
                      value = 0.05)
        },
        textOutput("pcaGeneCountPreview"),
        materialSwitch(inputId = "pcCenter", label = "Center", value = TRUE, right = TRUE, status = "primary"),
        materialSwitch(inputId = "pcScale", label = "Scale", value = TRUE, right = TRUE, status = "primary"),
        materialSwitch(inputId = "pcTransform", label = "Log(x+1) transform", value = TRUE, right = TRUE, status = "primary"),
        radioGroupButtons(
          "pcData",
          "Source",
          choices = c("Original" = "o",
                      "Normalized" = "n"), 
          justified = TRUE,
          status = "primary"
        ),
        # selectInput(
        #   "dendMethod",
        #   "Hierarchical Clustering Method",
        #   choices = list(
        #     "ward.D" = "ward.D",
        #     "ward.D2" = "ward.D2",
        #     "Single" = "single",
        #     "Complete" = "complete",
        #     "UPGMA" = "average",
        #     "WPGMA" = "mcquitty",
        #     "WOGMC" = "median",
        #     "UPGMC" = "centroid"
        #   )
        # ),
        do.call(actionBttn, c(
          list(
            inputId = "pcRun",
            label = "Run PCA",
            icon = icon("play")
          ),
          actionBttnParams
        ))
      )
    })
  }
})


# Preview gene count -----
observeEvent(input$pcFDR, {
  gene_count <- nrow(resultTable()[resultTable()$q.value <= input$pcFDR, ])
  output$pcaGeneCountPreview <- renderText({
    paste0("Gene number: ", gene_count)
  })
})

# ====================================
# If [Run PCA] button has been clicked, then run the whole PCA Analysis.
# Position: In [PCA Analysis tab], upper left. 
# ====================================
# Input: pcRun (Button)
# Output: All PCA output
# ====================================
observeEvent(input$pcRun, {
  # Select Sample (Column)
  # Grouping.
  data.cl <- variables$groupListConvert
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
  if (input$pcTransform == TRUE) {
    data <- t(log1p(data))
  } else {
    data <- t(data)
  }
  data.pca <- prcomp(data[ , apply(data, 2, var) != 0],
                     center = input$pcCenter,
                     scale. = input$pcScale) 
  
  # Scree Plot plotly object ----
  summaryTable <- summary(data.pca)$importance
  
  output$pcaVariances <-renderPlotly({
    p <- plot_ly(x = colnames(summaryTable), 
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
             yaxis = list(title = "Proportion of Variance", 
                          tickformat = "%"),
             title = "Scree Plot",
             legend = list(
               orientation = 'h',
               xanchor = "center",
               x = 0.5,
               y = 1
             ))
    variables$screePlot <- p 
    p 
  })
  
  # Scatter Plot 2D plotly object ----
  output$pcabiplot <- renderPlotly({
    p <- plot_ly(
      data = data.frame(data.pca$x),
      x = ~ PC1,
      y = ~ PC2,
      color = as.factor(data.cl),
      text = row.names(data.pca$x),
      textposition = "top right",
      type = "scatter",
      mode = "markers+text"
    ) %>%
      layout(title = "PCA Plot (2D)",
             legend = list(
               orientation = 'h',
               xanchor = "center",
               x = 0.5,
               y = 1
             ))
    variables$pca2d <- p
    p
  })
  
  # Scatter Plot 3D plotly object ----
  output$pcabiplot3d <- renderPlotly({
    p <- plot_ly(
      data = data.frame(data.pca$x),
      x = ~ PC1,
      y = ~ PC2,
      z = ~ PC3,
      color = as.factor(data.cl),
      text = row.names(data.pca$x),
      textposition = "top right",
      type = "scatter3d",
      mode = "markers+text"
    ) %>%
      layout(title = "PCA Plot (3D)",
             legend = list(
               orientation = 'h',
               xanchor = "center",
               x = 0.5,
               y = 1
             ))
    variables$pca3d <- p
    p
  })
  
  # Cluster Dendrogram
  # data.cluster <- hclust(dist(data.pca$x),method = input$dendMethod)
  # 
  # output$pcacluster <- renderPlot(
  #   plot(data.cluster, xlab = "Sample")
  # )
  
  # Summary Table
  output$summaryPCA <- DT::renderDataTable({
    row.names(summaryTable)[1] <- "Standard Deviation"
    DT::datatable(summaryTable, options = list(dom = "t")) %>% 
      formatRound(
        columns = colnames(summaryTable),
      digits = 3)
  })
  
  output$runPCACode <- renderText({
    variables$runPCACode
  })
})