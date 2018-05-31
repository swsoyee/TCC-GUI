# server-ma-plot.R

# Generate MA Plot Parameters
observeEvent(input$TCC, {
  showNotification("Generate MA Plot Parameters.", type = "message")
  output$MAPlotParameter <- renderUI({
    tagList(
      selectInput("GeneAttribute", "Hover info:", choices = colnames(resultTable())),
      sliderInput("pointSize", "Point Size:", min = 1, max = 5, value = 3, step = 0.2),
      sliderInput("maFDR", "FDR:", min = 0, max = 1, value = input$fdr),
      colourInput("fdrColor", "DEGs color：", "#B22222"),
      fluidRow(column(6, actionButton("makeMAPlot", "Generate MA-Plot")),
               column(6, uiOutput("runMAPlot")))
      
    )
  })
})

observeEvent(input$makeMAPlot, {
  withProgress(message = 'MA Ploting: ', value = 0, {
    output$maploty <- renderPlotly({
      validate(
        need(resultTable()$a.value != "", "No MA values for ploting.")
      )
      
      req(input$makeMAPlot)
      isolate({
        key <- row.names(resultTable())
        
        if(is.null(input$resultTableInPlot_rows_selected)) {
          annotation <- list()
        } else {
          markerSelect <- resultTable()[input$resultTableInPlot_rows_selected,]
          
          annotation <- list(
            x = markerSelect$a.value,
            y = markerSelect$m.value,
            text = markerSelect[, input$GeneAttribute],
            xref = "x",
            yref = "y",
            showarrow = TRUE,
            arrowhead = 7,
            ax = 20,
            ay = 40
          )
        }
      
      if(input$testMethod != "wad"){
        x <- cut(resultTable()$q.value, breaks = c(0, input$maFDR, 1))
        levels(x) <- list("DEG"=paste("(0,", input$maFDR,"]", sep = ""),
                          "non-DEG"=paste("(", input$maFDR, ",1]", sep = ""))
        incProgress(0.5, detail = "Ploting...")
        plot_ly(data = resultTable(),
                x = ~a.value,
                y = ~m.value,
                type = "scatter",
                mode = "markers",
                color = ~x,
                colors = c(input$fdrColor, "#000000"),
                marker = list(size = 3),
                hoverinfo = "text",
                text = ~paste("</br>Gene:", resultTable()[, input$GeneAttribute],
                              "</br>A value:", round(a.value, 4),
                              "</br>M value:", round(m.value, 4),
                              "</br>Rank:", rank),
                source = "ma") %>%
          layout(xaxis = list(title = "A = (log2(G2)+log2(G1))/2"),
                 yaxis = list(title = "M = log2(G2)-log2(G1)"),
                 title = paste("MA Plot with FDR <", input$maFDR),
                 annotations = annotation)
      } else {
        incProgress(0.5, detail = "Ploting...")
        
        plot_ly(data = resultTable(),
                x = ~as.numeric(a.value),
                y = ~as.numeric(m.value),
                type = "scatter",
                mode = "markers",
                colors = c("#000000"),
                marker = list(size = 3),
                hoverinfo = "text",
                text = ~paste("</br>Gene:", resultTable()[, input$GeneAttribute],
                              "</br>A value:", round(as.numeric(a.value), 4),
                              "</br>M value:", round(as.numeric(m.value), 4),
                              "</br>Rank:", rank),
                source = "ma") %>%
          layout(xaxis = list(title = "A = (log2(G2)+log2(G1))/2"),
                 yaxis = list(title = "M = log2(G2)-log2(G1)"),
                 title = "MA Plot",
                 annotations = annotation)
      }
      })
    })
  })
  
  # ====================================
  # This function render a button of R code of making MA plot.
  # Position: In Volcano Plot, upper left, parameter panel.
  # ====================================
  
  output$runMAPlot <- renderUI({
    actionButton("showMACode", "Show R code")
  })
})

# ====================================
# This function popup a window of R code of making MA plot.
# Position: In MA Plot, middle.
# ====================================

observeEvent(input$showMACode, {
  shinyalert(
    title = "MA Plot code",
    text = variables$runMAPlot,
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "info",
    showConfirmButton = TRUE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    cancelButtonText = "Close",
    animation = TRUE
  )
})

# When hover on the point, show a expresion plot of specific gene.
output$geneBarPlot <- renderPlotly({
  # Read in hover data
  eventdata <- event_data("plotly_hover", source = "ma")
  validate(need(!is.null(eventdata), 
                "Hover over the point to show expression plot"))
  # Get point number
  datapoint <- as.numeric(eventdata$pointNumber)[1]
  # Get expression level (Original)
  expression <- variables$CountData[datapoint, ]
  # Get expression level (Normalized)
  expressionNor <- t(t(variables$norData[datapoint, ]))
  
  data <- variables$CountData
  data.cl <- rep(0, ncol(data))
  
  for (i in 1:length(variables$groupList)) {
    data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = data))] = i
  }
  
  expression <- t(expression[data.cl != 0])
  data.cl <- data.cl[data.cl != 0]
  
  plot_ly(x = ~row.names(expression),
          y = ~expression[, 1],
          color = as.factor(data.cl),
          text = expression[, 1], 
          textposition = 'auto',
          type = "bar",
          name = "Original") %>%
    add_trace(y = ~expressionNor[, 1],
              text = round(expressionNor[, 1], 2), 
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


output$resultTableInVolcanalPlot <- output$resultTableInPlot <- DT::renderDataTable({
  if (nrow(resultTable()) == 0) {
    DT::datatable(resultTable())
  } else {
    DT::datatable(
      resultTable(),
      option = list(
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
      )
    ) %>% formatRound(
      columns = c("a.value",
                  "m.value",
                  "p.value",
                  "q.value"),
      digits = 3
    )
  }
})

# ====================================
# This function render a table of different gene count under specific FDR cutoff
# condition.
# Position: In MA plot tab, under left.
# ====================================

output$fdrCutoffTableInMAPage <- DT::renderDataTable({
  # Create Table
  df <- make_summary_for_tcc_result(resultTable())
  
  # Render Table
  DT::datatable(df[, c("Cutoff", "Count", "Percentage")],
                option = list(
                  pageLength = 10,
                  columnDefs = list(list(
                    className = 'dt-right', targets = "_all"
                  )),
                  dom = "tp"
                ),
                rownames = FALSE)
})

# ====================================
# This function render a plotly of different gene count under specific FDR cutoff
# condition.
# Position: In MA plot tab, under left.
# ====================================
output$fdrCutoffPlotInMAPage <- renderPlotly({
  # Create table
  df <- make_summary_for_tcc_result(resultTable())
  
  # Render Plotly
  plot_ly(data = df,
          x = ~as.numeric(Cutoff),
          y = ~Between_Count,
          type = "bar",
          hoverinfo = "text",
          text = ~paste("</br>FDR Cutoff: ", Cutoff,
                        "</br>DEGs Count: ", Between_Count)) %>%
    add_trace(y = ~Under_Count,
              yaxis = "y2",
              type = "scatter",
              mode = "lines+markers",
              hoverinfo = "text",
              text = ~paste("</br>FDR Cutoff: ", Cutoff,
                            "</br>Cumulative curve: ", Percentage)) %>%
    layout(xaxis = list(title = "FDR Cutoff"),
           yaxis = list(title = "DEGs Count"),
           yaxis2 = list(overlaying = "y", side = "right"),
           showlegend = FALSE)
})