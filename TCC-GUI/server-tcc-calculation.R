# server-tcc-calculation.R

observeEvent(input$TCC, {
  withProgress(message = 'TCC Calculation: ', value = 0, {
    # Set time start
    start_time <- Sys.time()

    data <- variables$CountData
    
    data.cl <- rep(0, ncol(data))
    convert2cl <- function(x, df) {
      grep(x, colnames(df))
    }
    
    for (i in 1:length(variables$groupList)) {
      data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = data))] = i
    }

    incProgress(0.2, detail = "Creating TCC Object...")
    tcc <- new("TCC", data[data.cl != 0], data.cl[data.cl != 0])
    tcc <- filterLowCountGenes(tcc, low.count = input$filterLowCount)
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
    variables$norData <- tcc$getNormalizedData()
    variables$runTimes <- variables$runTimes + 1
    # Show computation time notification
    # Set time end
    end_time <- Sys.time()
    runtime <- round(as.numeric(end_time - start_time), 3)
    showNotification(paste("Running time:", runtime, "seconds"), type = "message")
  })
  
  output$resultTable <- DT::renderDataTable({
    if (nrow(variables$result) == 0) {
      DT::datatable(variables$result)
    } else {
      DT::datatable(
        variables$result,
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
  
  # ====================================
  # This function render a table of different gene count under specific FDR cutoff
  # condition.
  # Position: In Computation tab, under right.
  # ====================================
  
  output$fdrCutoffTableInTCC <- DT::renderDataTable({
    # Create Table
    df <- make_summary_for_tcc_result(variables$result)
    
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
  # Position: In Computation tab, under right.
  # ====================================
  output$fdrCutoffPlotInTCC <- renderPlotly({
    # Create table
    df <- make_summary_for_tcc_result(variables$result)
    
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
  
  # Download TCC Normalized Table function
  output$downLoadNormalized <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), 
            input$normMethod,
            input$testMethod,
            input$iteration,
            input$fdr,
            input$floorpdeg, "TCC_Normalized.csv", sep = "_")
    },
    content = function(file) {
      write.csv(variables$norData, file)
    }
  )
  
  # observeEvent(input$TCC, {
    output$mainResultTable <- renderUI({
      tagList(
        tags$hr(),
        # tags$h3("Result Table"),
        # Generate Result file download button
        fluidRow(column(
          3,
          downloadButton("downLoadResultTable", "Download TCC Result")
        ),
        column(
          3,
          downloadButton("downLoadNormalized", "Download Normalized Data")
        )),
        DT::dataTableOutput('resultTable')
      )
    })
    
    output$runTCCCode <- renderUI({
      actionButton("showTCCCode", "Show R code")
    })
  # })
})

resultTable <- reactive({
  variables$result
})

# output$resultTable <- DT::renderDataTable({
#   if (nrow(resultTable()) == 0) {
#     DT::datatable(resultTable())
#   } else {
#     DT::datatable(
#       resultTable(),
#       option = list(
#         dom = "tpi",
#         pageLength = 10,
#         searchHighlight = TRUE,
#         orderClasses = TRUE
#       )
#     ) %>% formatRound(
#       columns = c("a.value",
#                   "m.value",
#                   "p.value",
#                   "q.value"),
#       digits = 3
#     )
#   }
# })

# observeEvent(input$TCC, {
#   output$mainResultTable <- renderUI({
#     tagList(
#       tags$hr(),
#       # tags$h3("Result Table"),
#       # Generate Result file download button
#       fluidRow(column(
#         3,
#         downloadButton("downLoadResultTable", "Download TCC Result")
#       ),
#       column(
#         3,
#         downloadButton("downLoadNormalized", "Download Normalized Data")
#       )),
#       DT::dataTableOutput('resultTable')
#     )
#   })
#   
#   output$runTCCCode <- renderUI({
#     actionButton("showTCCCode", "Show R code")
#   })
# })
# 
observeEvent(input$showTCCCode, {
  shinyalert(
    title = "TCC Run code",
    text = variables$runTCCCode,
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
# # Download TCC Result Table function
# output$downLoadResultTable <- downloadHandler(
#   filename = function() {
#     paste(Sys.Date(), 
#           input$normMethod,
#           input$testMethod,
#           input$iteration,
#           input$fdr,
#           input$floorpdeg, "TCC.csv", sep = "_")
#   },
#   content = function(file) {
#     write.csv(resultTable(), file, row.names = FALSE)
#   }
# )
# 
# # Download TCC Normalized Table function
# output$downLoadNormalized <- downloadHandler(
#   filename = function() {
#     paste(Sys.Date(), 
#           input$normMethod,
#           input$testMethod,
#           input$iteration,
#           input$fdr,
#           input$floorpdeg, "TCC_Normalized.csv", sep = "_")
#   },
#   content = function(file) {
#     write.csv(variables$norData, file)
#   }
# )

# # ====================================
# # This function render a table of different gene count under specific FDR cutoff
# # condition.
# # Position: In Computation tab, under right.
# # ====================================
# 
# output$fdrCutoffTableInTCC <- DT::renderDataTable({
#   # Create Table
#   df <- make_summary_for_tcc_result(resultTable())
#   
#   # Render Table
#   DT::datatable(df[, c("Cutoff", "Count", "Percentage")],
#                 option = list(
#                   pageLength = 10,
#                   columnDefs = list(list(
#                     className = 'dt-right', targets = "_all"
#                   )),
#                   dom = "tp"
#                 ),
#                 rownames = FALSE)
# })
# 
# # ====================================
# # This function render a plotly of different gene count under specific FDR cutoff
# # condition.
# # Position: In Computation tab, under right.
# # ====================================
# output$fdrCutoffPlotInTCC <- renderPlotly({
#   # Create table
#   df <- make_summary_for_tcc_result(resultTable())
# 
#   # Render Plotly
#   plot_ly(data = df,
#           x = ~as.numeric(Cutoff),
#           y = ~Between_Count,
#           type = "bar",
#           hoverinfo = "text",
#           text = ~paste("</br>FDR Cutoff: ", Cutoff,
#                         "</br>DEGs Count: ", Between_Count)) %>%
#     add_trace(y = ~Under_Count,
#               yaxis = "y2",
#               type = "scatter",
#               mode = "lines+markers",
#               hoverinfo = "text",
#               text = ~paste("</br>FDR Cutoff: ", Cutoff,
#                             "</br>Cumulative curve: ", Percentage)) %>%
#     layout(xaxis = list(title = "FDR Cutoff"),
#            yaxis = list(title = "DEGs Count"),
#            yaxis2 = list(overlaying = "y", side = "right"),
#            showlegend = FALSE)
# })