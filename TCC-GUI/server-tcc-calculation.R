# server-tcc-calculation.R

observeEvent(input$sider, {
  if (input$sider == "calculationTab") {
    data <- variables$CountData
    data.cl <- unlist(variables$groupList)
    tryCatch({
      output$lowCountFilterByCutoff <- renderPlotly({
        
        cData <- data[, data.cl %in% colnames(data)]
        lowCount <- sapply(0:input$lowCountSlide, function(x){sum(rowSums(cData) <= x)})
        
        lowCountdt <- data.frame(
          "Cutoff" = 0:input$lowCountSlide,
          "Filtered" = lowCount,
          "Remain" = nrow(cData) - lowCount
        )
        
        plot_ly(
          lowCountdt,
          name = "Filtered",
          x =  ~ Cutoff,
          y =  ~ Filtered,
          hoverinfo = "text+name",
          hovertext = ~ paste0(
            "Cut off: ",
            Cutoff,
            "<br>Filtered number: ",
            Filtered,
            "<br>Remain number: ",
            Remain,
            "(",
            round(Remain / nrow(data) * 100, 2),
            "%)"
          ),
          type = "bar"
        ) %>% add_trace(
          name = "Remain",
          y =  ~ Remain,
          yaxis = "y2",
          type = "scatter",
          mode = "lines"
        )  %>% layout(
          title = "Filtering Threshold for Low Count Genes",
          xaxis = list(title = "Filtering Low Count Cut off"),
          yaxis = list(title = "Filtered number", 
                       titlefont = list(color = "#1F77B4"),
                       autorange = FALSE,
                       range = c(0, 2*max(lowCountdt$Filtered))
                       ),
          yaxis2 = list(title = "Remain number",
                        titlefont = list(color = "#FF7F0E"),
                        overlaying = "y", 
                        rangemode = "tozero",
                        side = "right")
        )
      })
    })
  }
})

# If the run TCC botton has been clicked, execute TCC calculation ---------

tccRun <- reactiveValues(tccRunValue = FALSE)

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
  if(input$filterLowCount != -1){
    tcc <-
      filterLowCountGenes(tcc, low.count = input$filterLowCount)
  }
  # Filtered number preview ----
  output$lowCountFilterText <- renderText({
    filtered <- nrow(data) - nrow(tcc$count)
    paste0(filtered, " genes (", 100 * (filtered / nrow(data)) ,"%) have been filtered out.")
  })
  
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
  variables$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character)
  
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
  runtime <- round(tcc$DEGES$execution.time[3], 2)
  
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Rendering tables",
    value = 93
  )
  
  # Render TCC result table on the right top ----
  output$resultTable <- DT::renderDataTable({
    if (nrow(variables$result) == 0) {
      DT::datatable(variables$result)
    } else {
      DT::datatable(
        variables$result,
        filter = "bottom",
        colnames = c("Gene Name",
                     "A Value",
                     "M Value",
                     "P Value",
                     "Q Value (FDR)",
                     "Rank",
                     "estimated DEG"),
        caption = tags$caption(
          tags$li(
            tags$b("Copy"),
            ", ",
            tags$b("Print"),
            " and ",
            tags$b("Download"),
            " buttons only deal with loaded part of the whole table (max to 99 rows)."
          ),
          tags$li(
            HTML("<font color=\"#B22222\"><b>Gene Name</b></font> is colored according to FDR cut-off.")
          )
        ),
        extensions = c("Scroller", "RowReorder", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
          rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = -1)
          )
        )
      ) %>% formatRound(
        columns = c("a.value",
                    "m.value",
                    "p.value",
                    "q.value"),
        digits = 3
      ) %>% formatStyle(
        "gene_id",
        "estimatedDEG",
        color = styleEqual(1, "#B22222"),
        fontWeight = styleEqual(c(0, 1), c("normal", "bold"))
      )
    }
  })
  
  
  # Render a table of norm.factors and lib.sizes ----
  output$tccSummation <- DT::renderDataTable({
    df <-
      data.frame(
        tcc$group,
        tcc$norm.factors,
        colSums(tcc$count),
        tcc$norm.factors * colSums(tcc$count)
      )
    colnames(df) <-
      c(
        "Group",
        "Normalization Factor",
        "Library Size<sup>*1</sup>",
        "Effective Library Size<sup>*2</sup>"
      )
    DT::datatable(df,
                  escape = FALSE,
                  extensions = "Buttons",
                  caption = tags$caption(
                    tags$li("Library Size",
                            tags$sup("*1"),
                            "= Sum of Raw Count."),
                    tags$li(
                      "Effective Library Size",
                      tags$sup("*2"),
                      " = Library Size × Normalization Factor."
                    )
                  ), 
                  option = list(dom = "Bt",
                                buttons = list(
                                  'copy',
                                  'print',
                                  list(
                                    extend = 'collection',
                                    buttons = c('csv', 'excel', 'pdf'),
                                    text = 'Download'
                                  )
                                ))) %>% formatRound(
                                  columns = c(
                                    "Normalization Factor",
                                    "Library Size<sup>*1</sup>",
                                    "Effective Library Size<sup>*2</sup>"
                                  ),
                                  digits = c(3, 0, 0)
                                ) %>% formatStyle(
                                  "Normalization Factor",
                                  background = styleColorBar(range(0, df[, "Normalization Factor"]), 'lightblue'),
                                  backgroundSize = '98% 88%',
                                  backgroundRepeat = 'no-repeat',
                                  backgroundPosition = 'center'
                                ) %>% formatStyle(
                                  "Library Size<sup>*1</sup>",
                                  background = styleColorBar(range(0, df[, "Library Size<sup>*1</sup>"]), 'lightblue'),
                                  backgroundSize = '98% 88%',
                                  backgroundRepeat = 'no-repeat',
                                  backgroundPosition = 'center'
                                ) %>% formatStyle(
                                  "Effective Library Size<sup>*2</sup>",
                                  background = styleColorBar(range(0, df[, "Effective Library Size<sup>*2</sup>"]), 'lightblue'),
                                  backgroundSize = '98% 88%',
                                  backgroundRepeat = 'no-repeat',
                                  backgroundPosition = 'center'
                                )
  })
  
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "Rendering plots",
    value = 97
  )
  
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

  # Render a boxplot of normalized sample distribution ----
  
  # withBars(output$NormalizedSampleDistribution <- renderPlotly({
  #   validate(need(
  #     colnames(variables$norData) %in% colnames(variables$CountData),
  #     "Please rerun the TCC."
  #   ))
  #   cpm_stack <-
  #     data.frame(stack(log2(variables$norData)))
  #   # Add a group column in case of bugs.
  #   cpm_stack$group <- 0
  #   # Add Group info
  #   for (i in 1:length(variables$groupList)) {
  #     cpm_stack[is.element(cpm_stack$col, variables$groupList[[i]]),]$group <- names(variables$groupList[i])
  #   }
  #   cpm_stack_order <-
  #     unique(cpm_stack[order(cpm_stack$group),]$col)
  #   xform <- list(
  #     categoryorder = "array",
  #     categoryarray = cpm_stack_order,
  #     title = input$sampleDistributionXlab
  #   )
  #   
  #   showNotification("Ploting normalized sample distribution", type = "message")
  # 
  #   plot_ly(
  #     x = cpm_stack[, 2],
  #     y = cpm_stack[, 4],
  #     type = "box",
  #     split = cpm_stack$group,
  #     color = cpm_stack$group
  #   ) %>%
  #     layout(
  #       title = "Normalized Count",
  #       xaxis = xform,
  #       yaxis = list(title = "log2(Count)"),
  #       legend = list(
  #         orientation = 'h',
  #         xanchor = "center",
  #         x = 0.5,
  #         y = input$sampleDistributionLegendY
  #       )
  #     )
  # }))
  
  # Render a density plot of normalized sample distribution ----
  
  output$NormalizedSampleDistributionDensity <- renderPlotly({
    
    cpm <- log2(variables$norData + 1)
    densityTable <-lapply(data.frame(cpm),  function(x) {density(x)})
    p <- plot_ly(type = "scatter", mode = "lines")
    
    for(i in 1:length(densityTable)){
      # Color group definition
      group <- sapply(variables$groupList, function(x) {names(densityTable[i]) %in% x})
      
      p <- add_trace(p, x = densityTable[[i]][[1]],
                     y = densityTable[[i]][[2]],
                     color = names(group[group]),
                     name = names(densityTable[i]))
    }
    p %>%
      layout(title = input$norDistributionDenstityTitle,
             xaxis = list(title = input$norDistributionDensityXlab),
             yaxis = list(title = input$norDistributionDensityYlab))
  })
  
  updateProgressBar(
    session = session,
    id = "tccCalculationProgress",
    title = "All done.",
    value = 100
  )
  
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "TCC was successfully performed.",
                 type = "success")
  
  tccRun$tccRunValue <- input$TCC
})

resultTable <- reactive({
  variables$result
})


# This function check the `Show R code` button, if the botton is clicked. ----

observeEvent(input$TCC, {
  output$showTCCCode <- renderText({
    variables$runTCCCode
  })
})

# This function render a series UI of Result table. ----

output$mainResultTable <- renderUI({
  if(tccRun$tccRunValue){
  tagList(fluidRow(column(
    12,
    downloadButton("downLoadResultTable", "Download All Result (CSV)"),
    downloadButton("downLoadNormalized", "Download Normalized Data (CSV)")
  )),
  tags$br(),
  fluidRow(column(
    12, DT::dataTableOutput('resultTable')
  )))} else {
    helpText("Click [Run TCC Calculation] to execute TCC computation first.")
  }
})

# Runder tcc summary table ----
output$tccSummationUI <- renderUI({
  if(tccRun$tccRunValue){
  tagList(
    DT::dataTableOutput("tccSummation")
  )} else {
    helpText("Summarization of TCC normalization will be shown after TCC computation.")
  }
})

output$norDistributionDensityPanel <- renderUI({
  if (tccRun$tccRunValue) {
    tagList(fluidRow(
      column(
        2,
        textInput(
          inputId = "norDistributionDenstityTitle",
          label = "Title",
          value = "Normalized Count",
          placeholder = "Normalized Count"
        ),
        textInput(
          inputId = "norDistributionDensityXlab",
          label = "X label",
          value = "log<sub>2</sub>(Count<sub>nor</sub> + 1)",
          placeholder = "log<sub>2</sub>(Count<sub>nor</sub> + 1)"
        ),
        textInput(
          inputId = "norDistributionDensityYlab",
          label = "Y label",
          value = "Density",
          placeholder = "Density"
        )
      ),
      column(
        10,
        plotlyOutput("NormalizedSampleDistributionDensity") %>% withSpinner()
      )
    ))
  } else {
    helpText("Click [Run TCC Calculation] to execute TCC computation first.")
  }
})