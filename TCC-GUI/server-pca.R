# server-pca.R

runPCA <- reactiveValues(runPCAValue = FALSE)

observeEvent(input$sider, {
  if (input$sider == "pcaTab") {
    output$pcaParameter <- renderUI({
      tagList(
        if (input$testMethod != 'wad') {
          tipify(sliderInput(
            "pcFDR",
            "FDR Cut-off",
            min = 0.01,
            max = 1,
            value = 0.05
          ),
          title = "Genes under the FDR cut-off will be used for PCA. Set 1 for exploratory analysis with all genes.")
        },
        textOutput("pcaGeneCountPreview"),
        materialSwitch(
          inputId = "pcCenter",
          label = "Center",
          value = TRUE,
          right = TRUE,
          status = "primary"
        ),
        materialSwitch(
          inputId = "pcScale",
          label = "Scale",
          value = TRUE,
          right = TRUE,
          status = "primary"
        ),
        materialSwitch(
          inputId = "pcTransform",
          label = "Log(x+1) transform",
          value = TRUE,
          right = TRUE,
          status = "primary"
        ),
        radioGroupButtons(
          "pcData",
          "Source",
          choices = c("Original" = "o",
                      "Normalized" = "n"),
          justified = TRUE,
          status = "primary"
        ),
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
  gene_count <-
    nrow(resultTable()[resultTable()$q.value <= input$pcFDR, ])
  output$pcaGeneCountPreview <- renderText({
    paste0("Gene number: ", gene_count)
  })
})


# [Run PCA] button has been clicked, then run the whole PCA Analysis. ----

observeEvent(input$pcRun, {
  runPCA$runPCAValue <- input$pcRun
  variables$pcaParameter <- list("pcData" = input$pcData,
                                 "pcFDR" = input$pcFDR,
                                 "pcTransform" = input$pcTransform,
                                 "pcCenter" = input$pcCenter,
                                 "pcScale" = input$pcScale)
  tcc <- variables$tccObject
  
  # Using Original Dataset or Normalized Dataset.
  if (input$pcData == "o") {
    data <- tcc$count
  } else {
    data <- getNormalizedData(tcc)
  }
  
  result <- getResult(tcc)
  
  # Select DEGs (Row)
  if (input$testMethod == 'wad') {
    data <- data
  } else {
    data <- data[result$q.value <= input$pcFDR,]
  }
  
  # PCA processing
  if (input$pcTransform == TRUE) {
    data <- t(log1p(data))
  } else {
    data <- t(data)
  }
  data.pca <- prcomp(data[, apply(data, 2, var) != 0],
                     center = input$pcCenter,
                     scale. = input$pcScale)
  
  variables$data.pca <- data.pca
  
  output$runPCACode <- renderText({
    variables$runPCACode
  })
})

output$pcaVariances <- renderPlotly({
  if (length(variables$data.pca) > 0) {
    # Scree Plot plotly object ----
    data.pca <- variables$data.pca
    summaryTable <- summary(data.pca)$importance
    
    p <- plot_ly(
      x = colnames(summaryTable),
      y = summaryTable[2,],
      text = paste0(summaryTable[2,] * 100, "%"),
      textposition = "auto",
      type = "bar",
      name = "Proportion of Variance"
    ) %>%
      add_trace(
        y = summaryTable[3,],
        type = "scatter",
        mode = "lines+markers",
        name = "Cumulative Proportion"
      ) %>%
      layout(
        xaxis = list(title = "Principal Components"),
        yaxis = list(title = "Proportion of Variance",
                     tickformat = "%"),
        title = "Scree Plot",
        legend = list(
          orientation = 'h',
          xanchor = "center",
          x = 0.5,
          y = 1.05
        )
      )
    variables$screePlot <- p
    p
  } else {
    return()
  }
})

# Render Scree Plot UI ----
output$screePlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("pcaVariances") %>% withSpinner()
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})

# Render PCA 2d Plot ----
output$pca2d <- renderPlotly({
  if (length(variables$data.pca) > 0) {
    data.pca <- variables$data.pca
    data <- data.frame(data.pca$x)
    data$name <- rownames(data)
    group <- tcc$group
    group$name <- rownames(group)
    data <- left_join(x = data, y = group, by = "name")
    p <- plot_ly(
      data = data,
      x = ~ PC1,
      y = ~ PC2,
      color = ~ factor(group),
      text = ~ name,
      textposition = "top right",
      type = "scatter",
      mode = "markers+text"
    ) %>%
      layout(title = "PCA Plot (2D)")
    variables$pca2d <- p
    p
  } else {
    return()
  }
})

# Render 2D Plot UI ----
output$pca2dPlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("pca2d") %>% withSpinner()
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})

# Scatter Plot 3D plotly object ----
output$pca3d <- renderPlotly({
  if (length(variables$data.pca) > 0) {
    data.pca <- variables$data.pca
    data <- data.frame(data.pca$x)
    data$name <- rownames(data)
    group <- tcc$group
    group$name <- rownames(group)
    data <- left_join(x = data, y = group, by = "name")
    p <- plot_ly(
      data = data,
      x = ~ PC1,
      y = ~ PC2,
      z = ~ PC3,
      color = ~ factor(group),
      text = ~ name,
      textposition = "top right",
      type = "scatter3d",
      mode = "markers+text"
    ) %>%
      layout(title = "PCA Plot (3D)")
    variables$pca3d <- p
    p
  } else {
    return()
  }
})

# Render 3D Plot UI ----
output$pca3dPlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("pca3d") %>% withSpinner()
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})

# Summary Table ----
output$summaryPCA <- DT::renderDataTable({
  if (length(variables$data.pca) > 0) {
    data.pca <- variables$data.pca
    summaryTable <- summary(data.pca)$importance
    row.names(summaryTable)[1] <- "Standard Deviation"
    summaryTable <- t(summaryTable)
     t <- DT::datatable(summaryTable, options = list(
      dom = "Bt",
      buttons = list(
        'copy',
        'print',
        list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        )
      )
    )) %>%
      formatRound(columns = colnames(summaryTable),
                  digits = 3) %>% formatStyle(
                    "Proportion of Variance",
                    background = styleColorBar(range(0, 1), 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
                  ) %>% formatStyle(
                    "Standard Deviation",
                    background = styleColorBar(range(0, summaryTable[, 1]), 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
                  ) %>% formatStyle(
                    "Cumulative Proportion",
                    background = styleColorBar(range(0, 1), 'lightblue'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
                  )
    variables$summaryPCA <- t
  } else {
    return()
  }
})

# Render Summary Table ----
output$pcaSummaryTable <- renderUI({
  if (runPCA$runPCAValue) {
    DT::dataTableOutput("summaryPCA")
  } else {
    helpText("Click [Run PCA] to compute first.")
  }
})