# server-expression-plot.R

# ====================================
# This function render a selectInput in expression paramters box
# Position: In Expression Plot, upper left.
# ====================================
# Input: None
# Output: Textinput (widget)
# ====================================
output$expressionParameters <- renderUI({
  textAreaInput(
    "expressionGeneList",
    "Paste a list of genes",
    rows = 5,
    placeholder = "Input gene's name (first column in the dataset), one gene per line."
  )
})

# ====================================
# This function render a series output of expression level
# Position: In [Expression Plot tab], right.
# ====================================
# Input: expressionGeneList (Observe input gene list)
# Output: Plotly Object & DataTable Object
# ====================================
observeEvent(input$expressionGeneList, {
  data <- variables$CountData
  data.cl <- variables$groupListConvert

  data <-
    data[row.names(data) %in% unlist(strsplit(x = input$expressionGeneList, split = '[\r\n]')), ]
  data <- data[, data.cl != 0]
  data.cl <- data.cl[data.cl != 0]
  
  # ====================================
  # This function render a plotly of specific gene expression value in barplot.
  # Position: In [Expression Plot tab], upper right.
  # ====================================
  # Input: None
  # Output: Plotly object (Plot)
  # ====================================
  output$geneBarPlotExpression <- renderPlotly({
    validate(
      need(input$expressionGeneList != "", "Please select gene(s).")
    )
    p <- list(0)
    
    if(nrow(data) == 0) {
      showNotification("No data in your dataset! Please check your input!", type = "error")
      return()
    }
    
    xOrder <- data.frame("name" = row.names(data), "group" = data.cl)
    xOrderVector <- unique(xOrder[order(xOrder$group), ]$name)
    xform <- list(categoryorder = "array",
                  categoryarray = xOrderVector,
                  title = "")
    
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
          showlegend = FALSE,
          xaxis = xform
        )
    }
    subplot(p)
  })
  # ====================================
  # This function render a plotly of specific gene expression value in boxplot.
  # Position: In [Expression Plot tab], upper right.
  # ====================================
  # Input: None
  # Output: Plotly object (Plot)
  # ====================================
  
  output$geneBoxPlotExpression <- renderPlotly({
    validate(
      need(input$expressionGeneList != "", "Please select gene(s).")
    )
    p <- list(0)
    
    if(nrow(data) == 0) {
      showNotification("No data in your dataset! Please check your input!", type = "error")
      return()
    }
    for (i in 1:nrow(data)) {
      p[[i]] <- plot_ly(
        x = as.factor(data.cl),
        y = t(data[i, ]),
        color = as.factor(data.cl),
        type = "box",
        boxpoints = "all", 
        jitter = 0.3,
        pointpos = -1.8
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
  # This function render a Data.Table of specific gene expression level.
  # Position: In [Expression Plot tab], down right.
  # ====================================
  # Input: None
  # Output: DataTable object (Table)
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
  # Position: In [Expression Plot tab], down right.
  # ====================================
  # Input: None
  # Output: DataTable object (Table)
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
})