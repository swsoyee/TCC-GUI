# server-expression-plot.R

# ====================================
# This function render UI of expression plot paramters.
# Position: In Expression tab, top left.
# ====================================
# observeEvent(input$TCC, {
#   output$expressionParameter <- renderUI({
#     print("yes")
#     tagList(
#       selectInput("expression", "Select Gene(s):",
#                   choices = row.names(variables$CountData),
#                   multiple = TRUE)
#     )
#   })
# })

# ====================================
# This function render a plotly of specific gene expression value in barplot.
# Position: In Expression Plot, upper right.
# ====================================

# observeEvent(input$expressionGene, {
#   print(input$expressionGene)
  output$geneBarPlot <- renderPlotly({

    # Get expression level (Normalized)
    # expressionNor <- t(t(variables$norData[datapoint, ]))
    
    # Get expression level (Original)
    data <-  variables$CountData[row.names(variables$CountData) == input$expression, ]
    data.cl <- rep(0, ncol(data))

    for (i in 1:length(variables$groupList)) {
      data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = data))] = i
    }

    expression <- t(expression[data.cl != 0])
    data.cl <- data.cl[data.cl != 0]
    print(expression)
    
    plot_ly(x = ~row.names(expression),
            y = ~expression,
            color = as.factor(data.cl),
            text = expression,
            textposition = 'auto',
            type = "bar",
            name = "Original") %>%
      add_trace(y = ~expressionNor,
                text = round(expressionNor, 2),
                textposition = 'auto',
                name = "Normalized",
                type = "scatter",
                mode = "lines+markers",
                marker = list(size = 10,
                              line = list(color = 'rgba(0, 0, 0, 0)',
                                          width = 2))) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Raw Count"),
             title = paste(colnames(expression), "Expression Plot"),
             legend = list(orientation = 'h'))
  })
# })