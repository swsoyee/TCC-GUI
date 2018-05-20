# server-data-import.R
# 如果点击了加载sample data按键(input$CountDataSample)，加载sample
observeEvent(input$CountDataSample, {
  variables$CountData <- read.table(sample_data_url, 
                                    header = TRUE, 
                                    row.names = 1, 
                                    sep="\t", 
                                    quote="")
  print("Count data sample load")
})

# 如果点击了上传文件(input$uploadCountData)，则更新variables$CountData
observeEvent(input$uploadCountData, {
  print("Received uploaded file")
  variables$CountData <- read.table(input$uploadCountData$datapath,
                                    header = TRUE,
                                    row.names = 1,
                                    sep="\t", 
                                    quote="")
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
  print("Group number changed.")
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
  print("Log: Group information has been update.")
  print(groupList)
  variables$groupList <- groupList
})

# Generate TCC Parameters
observeEvent(input$confirmedGroupList, {
  print("Generate Test run TCC.")
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
      ),
      tags$hr(),
      plotlyOutput("degCutOffPlot")
      # DT::dataTableOutput("fdrCutoffTable")
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