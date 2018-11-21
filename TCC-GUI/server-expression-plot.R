# server-expression-plot.R

# ====================================
# This function render a selectInput in expression paramters box
# Position: In Expression Plot, upper left.
# ====================================
# Input: None
# Output: Textinput (widget)
# ====================================
output$expressionParameters <- renderUI({
  tagList(
    textAreaInput(
      "expressionGeneList",
      "Paste a list of genes",
      rows = 5,
      placeholder = "Input gene's name (first column in the dataset), one gene per line."
    ),
    do.call(actionBttn, c(
      list(
        inputId = "runExpression",
        label = "Generate Plot",
        icon = icon("play")
      ),
      actionBttnParams
    ))
  )
})

# ====================================
# This function render a series output of expression level
# Position: In [Expression Plot tab], right.
# ====================================
# Input: expressionGeneList (Observe input gene list)
# Output: Plotly Object & DataTable Object
# ====================================
observeEvent(input$runExpression, {
  
    data <- variables$CountData
    data.cl <- variables$groupListConvert
    
    data <-
      data[row.names(data) %in% unlist(strsplit(x = input$expressionGeneList, split = '[\r\n]')),]
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
      # validate(
      #   need(input$expressionGeneList != "", "Please select gene(s).")
      # )
      isolate({
      p <- list(0)
      
      if (input$expressionGeneList == "") {
        sendSweetAlert(
          session = session,
          title = "Gene list error!",
          text = "No such data in your dataset! Please check your input!",
          type = "error"
        )
        return()
      }
      
      xOrder <-
        data.frame("name" = colnames(data), "group" = data.cl)
      xOrderVector <- unique(xOrder[order(xOrder$group),]$name)
      xform <- list(
        categoryorder = "array",
        categoryarray = xOrderVector,
        title = ""
      )
      
      for (i in 1:nrow(data)) {
        p[[i]] <- plot_ly(
          x = colnames(data),
          y = t(data[i,]),
          color = as.factor(data.cl),
          type = "bar"
        ) %>%
          layout(
            annotations = list(
              x = 0.5,
              y = 1.05,
              text = row.names(data[i,]),
              showarrow = F,
              xref = 'paper',
              yref = 'paper'
            ),
            showlegend = FALSE,
            xaxis = xform
          )
      }
      f <- list(0)
      j <- 1
      for (i in seq(1, nrow(data), 2)) {
        if(i + 1 <= nrow(data)){
          f[[j]] <- subplot(p[[i]], p[[i+1]])
        } else {
          f[[j]] <- subplot(p[[i]])
        }
        j <- j + 1
      }
      subplot(f, nrows = j - 1, margin = 0.05)
      })
    })
    # ====================================
    # This function render a plotly of specific gene expression value in boxplot.
    # Position: In [Expression Plot tab], upper right.
    # ====================================
    # Input: None
    # Output: Plotly object (Plot)
    # ====================================
    
    output$geneBoxPlotExpression <- renderPlotly({
      # validate(
      #   need(input$expressionGeneList != "", "Please select gene(s).")
      # )
      isolate({
      p <- list(0)
      
      if (input$expressionGeneList == "") {
        sendSweetAlert(
          session = session,
          title = "Gene list error!",
          text = "No such data in your dataset! Please check your input!",
          type = "error"
        )
        return()
      }
      
      for (i in 1:nrow(data)) {
        p[[i]] <- plot_ly(
          x = as.factor(data.cl),
          y = t(data[i,]),
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
              text = row.names(data[i,]),
              showarrow = F,
              xref = 'paper',
              yref = 'paper'
            ),
            showlegend = FALSE
          )
      }
      f <- list(0)
      j <- 1
      for (i in seq(1, nrow(data), 2)) {
        if(i + 1 <= nrow(data)){
          f[[j]] <- subplot(p[[i]], p[[i+1]])
        } else {
          f[[j]] <- subplot(p[[i]])
        }
        j <- j + 1
      }
      subplot(f, nrows = j - 1, margin = 0.05)
      })
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
      clrs <-
        round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
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
      DT::datatable(resultTable()[resultTable()$gene_id %in% row.names(data), ],
                    options = list(dom = "t")) %>% formatRound(
                      columns = c("a.value",
                                  "m.value",
                                  "p.value",
                                  "q.value"),
                      digits = 3
                    )
    })

})