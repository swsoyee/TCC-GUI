# server-expression-plot.R

# ====================================
# This function render a plotly of specific gene expression value in barplot.
# Position: In Expression Plot, upper right.
# ====================================

output$geneBarPlotExpression <- renderPlotly({

  validate(
    need(input$expressionGene != "", "Please select gene(s).")
  )
  
  # Get expression level (Original)
  data <-
    variables$CountData[row.names(variables$CountData) %in% input$expressionGene,]
  data.cl <- rep(0, ncol(data))
  
  for (i in 1:length(variables$groupList)) {
    data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = data))] = i
  }
  data <- data[, data.cl != 0]
  data.cl <- data.cl[data.cl != 0]
  p <- list(0)
  
  for (i in 1:nrow(data)) {
    p[[i]] <- plot_ly(
      x = colnames(data),
      y = t(data[i, ]),
      color = as.factor(data.cl),
      type = "bar"
    ) %>%
      layout(
        annotations = list(
          x = 0.5,
          y = 1.05,
          text = row.names(data[i, ]),
          showarrow = F,
          xref = 'paper',
          yref = 'paper'
        ),
        showlegend = FALSE
      )
  }
  subplot(p)
})

# ====================================
# This function render a plotly of specific gene expression value in boxplot.
# Position: In Expression Plot, bottom right.
# ====================================

output$geneBoxPlotExpression <- renderPlotly({
  
  validate(
    need(input$expressionGene != "", "Please select gene(s).")
  )
  
  # Get expression level (Original)
  data <-
    variables$CountData[row.names(variables$CountData) %in% input$expressionGene,]
  data.cl <- rep(0, ncol(data))
  
  for (i in 1:length(variables$groupList)) {
    data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = data))] = i
  }
  data <- data[, data.cl != 0]
  data.cl <- data.cl[data.cl != 0]
  p <- list(0)
  
  for (i in 1:nrow(data)) {
    p[[i]] <- plot_ly(
      x = as.factor(data.cl),
      y = t(data[i, ]),
      color = as.factor(data.cl),
      type = "box"
    ) %>%
      layout(
        annotations = list(
          x = 0.5,
          y = 1.05,
          text = row.names(data[i, ]),
          showarrow = F,
          xref = 'paper',
          yref = 'paper'
        ),
        showlegend = FALSE
      )
  }
  subplot(p)
})