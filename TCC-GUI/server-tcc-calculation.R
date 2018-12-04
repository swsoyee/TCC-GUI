# server-tcc-calculation.R

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
  # tcc <- new("TCC", data[data.cl != 0], data.cl[data.cl != 0])
  tcc <- variables$tccObject
  # Filter low count genes before calculation
  if (input$filterLowCount != "Do not filter") {
    tcc$count <- tcc$count[rowSums(tcc$count) > as.numeric(input$filterLowCount), ]
  }
  # if(input$filterLowCount != -1){
  #   tcc <-
  #     filterLowCountGenes(tcc, low.count = input$filterLowCount)
  # }
  
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
  variables$tccObject <- tcc
  
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
          tags$li("Filter genes by typing condictions (such as 2...5) in the filter boxes to filter numeric columns. ",
            tags$b("Copy"),
            ", ",
            tags$b("Print"),
            " and ",
            tags$b("Download"),
            " the filtered result for further analysis."
          ),
          tags$li(
            HTML("<font color=\"#B22222\"><b>Gene Name</b></font> is colored according to FDR cut-off.")
          )
        ),
        extensions = c("Scroller", "Buttons"),
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
  }, server = FALSE)
  
  
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
    12, DT::dataTableOutput('resultTable') %>% withSpinner()
  )))} else {
    helpText("Click [Run TCC Computation] to obtain Result Table.")
  }
})

# Render tcc summary table ----
output$tccSummationUI <- renderUI({
  if(tccRun$tccRunValue){
  tagList(
    DT::dataTableOutput("tccSummation")
  )} else {
    helpText("Summary of TCC normalization will be shown after TCC computation.")
  }
})



# Filtered number preview ----
output$lowCountFilterText <- renderText({
  if (length(variables$tccObject) > 0) {
    # tcc <- variables$tccObject
    data <- variables$count.data
    if (input$filterLowCount != "Do not filter") {
      count <- data[rowSums(data) > as.numeric(input$filterLowCount), ]
    } else {
      count <- data
    }
    filtered <- nrow(data) - nrow(count)
    paste0(filtered,
           " genes (",
           round(100 * (filtered / nrow(data)),2) ,
           "%) will be filtered out.")
  } else {
    return()
  }
})