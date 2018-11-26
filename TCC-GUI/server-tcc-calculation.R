# server-tcc-calculation.R


# If the run TCC botton has been clicked, execute TCC calculation ---------


observeEvent(input$TCC, {
  progressSweetAlert(
    session = session,
    id = "tccCalculationProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "TCC computation",
    value = 10
  )
  # Set time start
  start_time <- Sys.time()
  
  data <- variables$CountData
  data.cl <- variables$groupListConvert
  
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Creating TCC Object",
    value = 20
  )
  # Create TCC Object
  tcc <- new("TCC", data[data.cl != 0], data.cl[data.cl != 0])
  # Filter low count genes before calculation
  tcc <-
    filterLowCountGenes(tcc, low.count = input$filterLowCount)
  
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Calculating normalization factors using DEGES",
    value = 50
  )
  # Run TCC and calculate normalized factor
  tcc <- calcNormFactors(
    tcc,
    norm.method = input$normMethod,
    test.method = input$testMethod,
    iteration = input$iteration,
    FDR = input$fdr,
    floorPDEG = input$floorpdeg
  )
  
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Identifying DE genes",
    value = 70
  )
  # Estimate DEGs
  tcc <- estimateDE(tcc,
                    test.method = input$testMethod,
                    FDR = input$fdr)
  # incProgress(1, detail = "Done.")
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Done",
    value = 80
  )
  # Get final result of TCC
  variables$result <- getResult(tcc, sort = FALSE)
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Save TCC computation result",
    value = 83
  )
  variables$norData <- tcc$getNormalizedData()
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Save normalized data",
    value = 87
  )
  
  # Show computation time notification
  # Set time end
  end_time <- Sys.time()
  runtime <- round(difftime(end_time, start_time, units = "secs"), 2)
  
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Rendering tables",
    value = 93
  )
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
  
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Rendering plots",
    value = 97
  )

  # Render a plotly of different gene count under specific FDR cutoff condition. ----
  
  output$fdrCutoffPlotInTCC <- renderPlotly({
    # Create table
    df <- make_summary_for_tcc_result(variables$result)
    
    # Render Plotly
    plot_ly(
      data = df,
      x = ~ Cutoff,
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
        xaxis = list(title = "FDR Cutoff" #,
                     #tickvals = 1:22,
                     #ticktext = Cutoff
                     ),
        yaxis = list(title = "DEGs Count"),
        yaxis2 = list(overlaying = "y", side = "right"),
        showlegend = FALSE
      )
  })
  
  # Download TCC Result Table function ----
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
  
  # Download TCC Normalized Table function ----
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
  
  

  # This function render a series UI of Result table. ----

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
  

  # Render a boxplot of normalized sample distribution ----
  
  withBars(output$NormalizedSampleDistribution <- renderPlotly({
    validate(need(
      colnames(variables$norData) %in% colnames(variables$CountData),
      "Please rerun the TCC."
    ))
    cpm_stack <-
      data.frame(stack(log2(variables$norData / 1000000)))
    # Add a group column in case of bugs.
    cpm_stack$group <- 0
    # Add Group info
    for (i in 1:length(variables$groupList)) {
      cpm_stack[is.element(cpm_stack$col, variables$groupList[[i]]),]$group <- names(variables$groupList[i])
    }
    cpm_stack_order <-
      unique(cpm_stack[order(cpm_stack$group),]$col)
    xform <- list(
      categoryorder = "array",
      categoryarray = cpm_stack_order,
      title = input$sampleDistributionXlab
    )
    
    showNotification("Ploting normalized sample distribution", type = "message")

    plot_ly(
      x = cpm_stack[, 2],
      y = cpm_stack[, 4],
      type = "box",
      split = cpm_stack$group
    ) %>%
      layout(
        title = "Normalized Sample Distribution",
        xaxis = xform,
        yaxis = list(title = "log2(CPM)"),
        legend = list(
          orientation = 'h',
          xanchor = "center",
          x = 0.5,
          y = input$sampleDistributionLegendY
        )
      )
  }))
  
  # Render a density plot of normalized sample distribution ----
  
  withBars(output$NormalizedSampleDistributionDensity <- renderPlotly({
    cpm <- log2(variables$norData/1000000)
    densityTable <-lapply(data.frame(cpm), density)
    p <- plot_ly(type = "scatter", mode = "lines")
    for(i in 1:length(densityTable)){
      p <- add_trace(p, x = densityTable[[i]][[1]],
                     y = densityTable[[i]][[2]],
                     # fill = "tozeroy",
                     name = names(densityTable[i]))
    }
    p %>%
      layout(title = "Normalized Sample Distribution",
             xaxis = list(title = "log2(CPM)"),
             yaxis = list(title = "Density"),
             legend = list(
               orientation = 'h',
               xanchor = "center",
               x = 0.5,
               y = input$sampleDistributionDensityLegendY
             ))
  }))
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "All done.",
    value = 100
  )
  
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "Calculation completed!",
                 type = "success")
})

resultTable <- reactive({
  variables$result
})

# ====================================
# This function check the `Show R code` button, if the botton is clicked,
# show the TCC running code.
# Position: In Computation tab, upper right, in TCC Parameters panel.
# ====================================

observeEvent(input$TCC, {
  output$showTCCCode <- renderText({
    variables$runTCCCode
  })
})