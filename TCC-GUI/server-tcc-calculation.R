# server-tcc-calculation.R

# ====================================
# This function check the `Run TCC` button, if the botton is clicked,
# run TCC calculation.
# Position: In Computation tab, upper right.
# ====================================

observeEvent(input$TCC, {
  withProgress(message = 'TCC Calculation: ', value = 0, {
    # Set time start
    start_time <- Sys.time()
    
    data <- variables$CountData
    data.cl<- variables$groupListConvert
    
    incProgress(0.2, detail = "Creating TCC Object...")
    # Create TCC Object
    tcc <- new("TCC", data[data.cl != 0], data.cl[data.cl != 0])
    # Filter low count genes before calculation
    tcc <-
      filterLowCountGenes(tcc, low.count = input$filterLowCount)
    incProgress(0.5, detail = "Calculating normalization factors using DEGES...")
    # Run TCC and calculate normalized factor
    tcc <- calcNormFactors(
      tcc,
      norm.method = input$normMethod,
      test.method = input$testMethod,
      iteration = input$iteration,
      FDR = input$fdr,
      floorPDEG = input$floorpdeg
    )
    incProgress(0.8, detail = "Identifying DE genes...")
    # Estimate DEGs
    tcc <- estimateDE(tcc,
                      test.method = input$testMethod,
                      FDR = input$fdr)
    incProgress(1, detail = "Done.")
    # Get final result of TCC
    variables$result <- getResult(tcc, sort = FALSE)
    variables$norData <- tcc$getNormalizedData()
    # Here is a switch, if this is the firs time of running TCC,
    # Render other Tabs
    variables$runTimes <- variables$runTimes + 1
    
    # Show computation time notification
    # Set time end
    end_time <- Sys.time()
    runtime <- round(as.numeric(end_time - start_time), 2)
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
    DT::datatable(
      df[, c("Cutoff", "Count", "Percentage")],
      option = list(
        pageLength = 10,
        columnDefs = list(list(
          className = 'dt-right', targets = "_all"
        )),
        dom = "tp"
      ),
      rownames = FALSE
    )
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
    plot_ly(
      data = df,
      x = ~ as.numeric(Cutoff),
      y = ~ Between_Count,
      type = "bar",
      hoverinfo = "text",
      text = ~ paste(
        "</br>FDR Cutoff: ",
        Cutoff,
        "</br>DEGs Count: ",
        Between_Count
      )
    ) %>%
      add_trace(
        y = ~ Under_Count,
        yaxis = "y2",
        type = "scatter",
        mode = "lines+markers",
        hoverinfo = "text",
        text = ~ paste(
          "</br>FDR Cutoff: ",
          Cutoff,
          "</br>Cumulative curve: ",
          Percentage
        )
      ) %>%
      layout(
        xaxis = list(title = "FDR Cutoff"),
        yaxis = list(title = "DEGs Count"),
        yaxis2 = list(overlaying = "y", side = "right"),
        showlegend = FALSE
      )
  })
  
  # Download TCC Result Table function
  output$downLoadResultTable <- downloadHandler(
    filename = function() {
      paste(
        Sys.Date(),
        input$normMethod,
        input$testMethod,
        input$iteration,
        input$fdr,
        input$floorpdeg,
        "TCC.csv",
        sep = "_"
      )
    },
    content = function(file) {
      write.csv(resultTable(), file, row.names = FALSE)
    }
  )
  
  # Download TCC Normalized Table function
  output$downLoadNormalized <- downloadHandler(
    filename = function() {
      paste(
        Sys.Date(),
        input$normMethod,
        input$testMethod,
        input$iteration,
        input$fdr,
        input$floorpdeg,
        "TCC_Normalized.csv",
        sep = "_"
      )
    },
    content = function(file) {
      write.csv(variables$norData, file)
    }
  )
  
  # ====================================
  # This function render a Sample Distribution.
  #
  # Position: In Computation tab, under middle.
  # ====================================
  output$sampleDistributionInTCC <- renderUI({
    plotlyOutput("NormalizedSampleDistribution")
  })
  
  
  # ====================================
  # This function render a series UI of Result table.
  #
  # Position: In Computation tab, under middle.
  # ====================================
  output$mainResultTable <- renderUI({
    tagList(fluidRow(column(
      3,
      downloadButton("downLoadResultTable", "Download TCC Result")
    ),
    column(
      3,
      downloadButton("downLoadNormalized", "Download Normalized Data")
    )),
    DT::dataTableOutput('resultTable'))
  })
  
  # ====================================
  # This function render a boxplot of normalized sample distribution
  #
  # Position: In Computation tab, middle middle.
  # ====================================
  
  output$NormalizedSampleDistribution <- renderPlotly({
    cpm_stack <- data.frame(stack(log2(variables$norData / 1000000)))
    # Add a group column in case of bugs.
    cpm_stack$group <- 0
    # Add Group info
    for (i in 1:length(variables$groupList)) {
      cpm_stack[is.element(cpm_stack$col, variables$groupList[[i]]), ]$group <-
        i
    }
    cpm_stack$group <- as.factor(cpm_stack$group)
    showNotification("Ploting Normalized Sample Distribution", type = "message")
    print(head(cpm_stack))
    plot_ly(
      x = cpm_stack[, 2],
      y = cpm_stack[, 4],
      type = "box",
      split = cpm_stack$group
    ) %>%
      layout(
        title = "Normalized Sample Distribution",
        xaxis = list(title = ""),
        yaxis = list(title = "log2 CPM")
      )
  })
  
  # ====================================
  # This function render a button of showing the TCC running code.
  #
  # Position: In Computation tab, under right, in TCC Parameters panel.
  # ====================================
  output$runTCCCode <- renderUI({
    actionButton("showTCCCode", "Show R code")
  })
})

resultTable <- reactive({
  variables$result
})

# ====================================
# This function check the `Show R code` button, if the botton is clicked,
# show the TCC running code.
# Position: In Computation tab, upper right, in TCC Parameters panel.
# ====================================

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