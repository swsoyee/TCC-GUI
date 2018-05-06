# server-ma-plot.R

# Generate MA Plot Parameters
observeEvent(input$TCC, {
  print("Generate MA Plot Parameters.")
  output$MAPlotParameter <- renderUI({
    tagList(
      selectInput("GeneAttribute", "Hover info：", choices = colnames(resultTable())),
      sliderInput("pointSize", "Point Size:", min = 1, max = 5, value = 3, step = 0.2),
      sliderInput("maFDR", "FDR:", min = 0, max = 1, value = input$fdr),
      actionButton("makeMAPlot", "Generate MA-Plot")
    )
  })
})

observeEvent(input$makeMAPlot, {
  withProgress(message = 'MA Ploting: ', value = 0, {
    output$maploty <- renderPlotly({
      
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
      
      if(input$testMethod != "WAD"){
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
                colors = c("#B22222", "#000000"),
                marker = list(size = 3),
                hoverinfo = "text",
                text = ~paste("</br>Gene:", resultTable()[, input$GeneAttribute],
                              "</br>A value:", round(a.value, 4),
                              "</br>M value:", round(m.value, 4),
                              "</br>Rank:", rank)) %>%
          layout(xaxis = list(title = "A = (log2(G2)+log2(G1))/2"),
                 yaxis = list(title = "M = log2(G2)-log2(G1)"),
                 title = paste("MA Plot with FDR <", input$maFDR),
                 annotations = annotation)
      } else {
        incProgress(0.5, detail = "Ploting...")
        plot_ly(data = resultTable(),
                x = ~a.value,
                y = ~m.value,
                type = "scatter",
                mode = "markers",
                colors = c("#000000"),
                marker = list(size = 3),
                hoverinfo = "text",
                text = ~paste("</br>Gene:", resultTable()[, input$GeneAttribute],
                              "</br>A value:", round(a.value, 4),
                              "</br>M value:", round(m.value, 4),
                              "</br>Rank:", rank)) %>%
          layout(xaxis = list(title = "A = (log2(G2)+log2(G1))/2"),
                 yaxis = list(title = "M = log2(G2)-log2(G1)"),
                 title = "MA Plot",
                 annotations = annotation)
      }
      })
    })
  })
})

output$resultTableInPlot <- DT::renderDataTable({
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

output$fdrCutoffTableInMAPage <- DT::renderDataTable({
  deg_in_cutoff <- sapply(c(0.01, seq(0.05, 1, 0.05)), sum_gene, resultTable())
  total_gene <- nrow(resultTable())
  DT::datatable(data.frame("FDR Cutoff" = c(0.01, seq(0.05, 1, 0.05)), 
                           "DEGs Count" = deg_in_cutoff,
                           "Percentage" = paste(deg_in_cutoff/total_gene * 100, "%")),
                option = list(
                  pageLength = 5,
                  dom = "tp"
                )
  )
})