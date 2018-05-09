# server-tcc-calculation.R

observeEvent(input$TCC, {
  withProgress(message = 'TCC Calculation: ', value = 0, {
    data <- variables$CountData
    
    data.cl <- rep(0, ncol(data))
    convert2cl <- function(x, df) {
      grep(x, colnames(df))
    }
    for (i in 1:length(variables$groupList)) {
      data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = data))] = i
    }
    
    incProgress(0.2, detail = "Creating TCC Object...")
    tcc <- new("TCC", data, data.cl)
    incProgress(0.5, detail = "Calculating normalization factors using DEGES...")
    tcc <- calcNormFactors(
      tcc,
      norm.method = input$normMethod,
      test.method = input$testMethod,
      iteration = input$iteration,
      FDR = input$fdr,
      floorPDEG = input$floorpdeg
    )
    incProgress(0.8, detail = "Identifying DE genes...")
    tcc <- estimateDE(tcc,
                      test.method = input$testMethod,
                      FDR = input$fdr)       #DEG検出を実行した結果をtccに格納
    incProgress(1, detail = "Done.")
    #p値などの計算結果をresultに格納
    variables$result <- getResult(tcc, sort = FALSE)
    
  })
})

resultTable <- reactive({
  variables$result
})

output$resultTable <- DT::renderDataTable({
  if (nrow(resultTable()) == 0) {
    DT::datatable(resultTable())
  } else {
    DT::datatable(
      resultTable(),
      option = list(
        dom = "tpi",
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

output$fdrCutoffTable <- DT::renderDataTable({
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

observeEvent(input$TCC, {
  output$mainResultTable <- renderUI({
    tagList(
      tags$hr(),
      # tags$h3("Result Table"),
      # Generate Result file download button
      downloadButton("downLoadResultTable", "Download TCC Result"),
      DT::dataTableOutput('resultTable')
    )
  })
})

# Download TCC Result Table function
output$downLoadResultTable <- downloadHandler(
  filename = function() {
    paste(Sys.Date(), 
          input$normMethod,
          input$testMethod,
          input$iteration,
          input$fdr,
          input$floorpdeg, "TCC.csv", sep = "_")
  },
  content = function(file) {
    write.csv(resultTable(), file, row.names = FALSE)
  }
)

output$degCutOffPlot <- renderPlotly({
  deg_in_cutoff <- sapply(c(0.01, seq(0.05, 1, 0.05)), sum_gene, resultTable())
  total_gene <- nrow(resultTable())
  df <- data.frame("FDR_Cutoff" = c(0.01, seq(0.05, 1, 0.05)), 
                   "DEGs_Count" = deg_in_cutoff,
                   "Percentage" = paste(deg_in_cutoff/total_gene * 100, "%"))
  plot_ly(data = df,
          x = ~FDR_Cutoff,
          y = ~DEGs_Count,
          type = "scatter",
          mode = "lines+markers",
          hoverinfo = "text",
          text = ~paste("</br>FDR Cutoff: ", FDR_Cutoff,
                        "</br>DEGs Count: ", DEGs_Count)) %>%
    layout(xaxis = list(title = "FDR Cutoff"),
           yaxis = list(title = "DEGs Count"))
})