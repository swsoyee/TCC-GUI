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
  
  # ====================================
  # This function render a Data.Table of specific gene expression level.
  # Position: In Expression Plot, bottom.
  # ====================================
  
  output$geneTable <- DT::renderDataTable({
    df <- data
    # Create 19 breaks and 20 rgb color values ranging from white to red
    brks <- quantile(df, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {
      paste0("rgb(255,", ., ",", ., ")")
    }
    
    DT::datatable(df,
                  options = list(dom = "t")) %>%
      formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
  })
  
  # ====================================
  # This function render a Data.Table of specific gene calculated result.
  # Position: In Expression Plot, bottom.
  # ====================================
  
  output$geneTableCal <- DT::renderDataTable({
    DT::datatable(resultTable()[resultTable()$gene_id %in% row.names(data),],
                  options = list(dom = "t")) %>% formatRound(
                    columns = c("a.value",
                                "m.value",
                                "p.value",
                                "q.value"),
                    digits = 3
                  )
  })
  
  # ====================================
  # This function render a plotly of specific gene expression value in boxplot.
  # Position: In Expression Plot, bottom right.
  # ====================================
  
  output$geneBoxPlotExpression <- renderPlotly({
    
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
  
  # ====================================
  # This function render a plotly of specific gene expression value in barplot.
  # Position: In Expression Plot, upper right.
  # ====================================
  
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

# # ====================================
# # This function render a plotly of specific gene expression value in boxplot.
# # Position: In Expression Plot, bottom right.
# # ====================================
# 
# output$geneBoxPlotExpression <- renderPlotly({
#   
#   validate(
#     need(input$expressionGene != "", "Please select gene(s).")
#   )
#   
#   # Get expression level (Original)
#   data <-
#     variables$CountData[row.names(variables$CountData) %in% input$expressionGene,]
#   data.cl <- rep(0, ncol(data))
#   
#   for (i in 1:length(variables$groupList)) {
#     data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = data))] = i
#   }
#   data <- data[, data.cl != 0]
#   data.cl <- data.cl[data.cl != 0]
#   p <- list(0)
#   
#   for (i in 1:nrow(data)) {
#     p[[i]] <- plot_ly(
#       x = as.factor(data.cl),
#       y = t(data[i, ]),
#       color = as.factor(data.cl),
#       type = "box"
#     ) %>%
#       layout(
#         annotations = list(
#           x = 0.5,
#           y = 1.05,
#           text = row.names(data[i, ]),
#           showarrow = F,
#           xref = 'paper',
#           yref = 'paper'
#         ),
#         showlegend = FALSE
#       )
#   }
#   subplot(p)
# })