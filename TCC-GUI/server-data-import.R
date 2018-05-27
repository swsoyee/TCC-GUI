# server-data-import.R

# ====================================
# If Sample Data button has been clicked, load sample data. 
# Position: In Computation tab, upper left. Button action.
# ====================================
# 2018-5-23 Change read.table to fread.

observeEvent(input$CountDataSample, {
  variables$CountData <- data.frame(fread(sample_data_url), row.names=1)
  showNotification("Count data sample load", type = "message")
})

# ====================================
# If Upload Data button has been clicked, load the data via upload. 
# Position: In Computation tab, upper left. Button action.
# ====================================
# 2018-5-23 Change read.table to fread.

observeEvent(input$uploadCountData, {
  showNotification("Received uploaded file", type = "message")
  variables$CountData <- data.frame(fread(input$uploadCountData$datapath), row.names=1)
})

datasetInput <- reactive({
  variables$CountData
})

output$table <- DT::renderDataTable({
  DT::datatable(datasetInput(),
                option = list(pageLength = 10,
                              searchHighlight = TRUE,
                              orderClasses = TRUE))
})

output$groupSlide <- renderUI({
  if(nrow(datasetInput())>0){
    tagList(
      sliderInput("groupNum", "Group Count", min = 2, max = 4, value = 2),
      actionButton("confirmedGroupNum", "Confirmed"),
      tags$hr()
    )
  }
})

observeEvent(input$confirmedGroupNum, {
  showNotification("Group number changed.", type = "message")
  colname <- colnames(datasetInput())
  names(colname) <- colname
  
  groupSelectList <- lapply(1:input$groupNum, function(x){
    output[[paste0("group", x)]] <- renderUI({
      selectInput(inputId = paste0("group", x),
                  label = paste0("Group", x),
                  choices = colname,
                  multiple = TRUE
      )
    })
  })
})

observeEvent(input$confirmedGroupNum, {
  output$confirmedGroupList <- renderUI({
    actionButton("confirmedGroupList", "Confirmed")
  })
})

observeEvent(input$confirmedGroupList, {
  variables$groupList <- list
  groupList <- lapply(1:input$groupNum, function(x){
    input[[paste0("group", x)]]
  })
  showNotification("Group information has been update.", type = "message")
  variables$groupList <- groupList
})

# Generate TCC Parameters
observeEvent(input$confirmedGroupList, {
  showNotification("Generate TCC Parameters.", type = "message")
  output$TCC <- renderUI({
    tagList(
      selectInput("normMethod", "Normalization method:",
                c("TMM" = "tmm",
                  "DESeq" = "deseq")),
      selectInput("testMethod", "DEGs identify method:",
                c("edgeR" = "edger",
                  "DESeq" = "deseq",
                  "DESeq2" = "deseq2",
                  "baySeq" = "bayseq",
                  "SAMSeq" = "samseq",
                  "Voom" = "voom",
                  "WAD" = "wad")),
      sliderInput("iteration", "Interation:", min = 1, max = 50, value = 3),
      sliderInput("fdr", "FDR:", min = 0, max = 1, value = 0.1, step = 0.05),
      sliderInput("floorpdeg", "Elimination of Potential DEGs:", min = 0, max = 1, value = 0.05, step = 0.05),
      fluidRow(
        column(6, actionButton("TCC", "Run TCC")),
        column(6, uiOutput("runTCCCode"))
      )
    )
  })
})

output$groupSelect <- renderUI({
  if(!is.null(input$groupNum)){
    tagList(
      lapply(1:input$groupNum, function(i) {
        uiOutput(paste0("group", i))
      })
    )
  }
})

# ====================================
# This function render a wellPanel (Table + Plotly) of different 
# gene count under specific FDR cutoff condition.
# Position: In Computation tab, under right.
# ====================================
observeEvent(input$TCC,{
  output$summaryTCC <- renderUI({
    tagList(
      wellPanel(
        tags$h4("FDR vs DEGs"),
        tags$hr(),
        tags$p("DEGs count under different FDR cutoff."),
        tabsetPanel(id = "maplot", 
                    tabPanel("Table", DT::dataTableOutput("fdrCutoffTableInTCC")),
                    tabPanel("Plot", plotlyOutput("fdrCutoffPlotInTCC")))
      )
    )
  })
})