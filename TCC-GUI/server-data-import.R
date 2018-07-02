# server-data-import.R

# ====================================
# If Sample Data button has been clicked, load sample data. 
# Position: In Computation tab, upper left. Button action.
# ====================================
# 2018-5-23 Change read.table to fread.

observeEvent(input$CountDataSample, {
  variables$CountData <-
    data.frame(fread(input$SampleDatabase), row.names = 1)
  showNotification("Count data sample load", type = "message")
  
  updateTextAreaInput(session, "groupSelectViaText", value = paste(
    "G1_rep1,1",
    "G1_rep2,1",
    "G1_rep3,1",
    "G2_rep1,2",
    "G2_rep2,2",
    "G2_rep3,2",
    sep = '\n'
  ))
  
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

# ====================================
# Render a table of raw count data, adding color.
# Position: In Computation tab, upper middle. 
# ====================================

output$table <- DT::renderDataTable({
  df <- datasetInput() 
  # Create 19 breaks and 20 rgb color values ranging from white to red
  brks <- quantile(df %>% select_if(is.numeric), probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  {
    paste0("rgb(255,", ., ",", ., ")")
  }
  
  DT::datatable(df,
                option = list(
                  scrollX = TRUE,
                  pageLength = 10,
                  searchHighlight = TRUE,
                  orderClasses = TRUE
                )) %>%
    formatStyle(names(df %>% select_if(is.numeric)), backgroundColor = styleInterval(brks, clrs))
})

# 2018-6-16 Unused
# output$groupSlide <- renderUI({
#   if(nrow(datasetInput())>0){
#     tagList(
#       sliderInput("groupNum", "Group Count", min = 2, max = 4, value = 2),
#       actionButton("confirmedGroupNum", "Confirmed"),
#       tags$hr()
#     )
#   }
# })

# 2018-6-16 Unused
# observeEvent(input$confirmedGroupNum, {
#   showNotification("Group number changed.", type = "message")
#   colname <- colnames(datasetInput())
#   names(colname) <- colname
#   
#   groupSelectList <- lapply(1:input$groupNum, function(x){
#     output[[paste0("group", x)]] <- renderUI({
#       selectInput(inputId = paste0("group", x),
#                   label = paste0("Group", x),
#                   choices = colname,
#                   multiple = TRUE
#       )
#     })
#   })
# })

# 2018-6-16 Unused
# observeEvent(input$confirmedGroupNum, {
#   output$confirmedGroupList <- renderUI({
#     actionButton("confirmedGroupList", "Confirmed")
#   })
# })

# 2018-6-16 Unused
# observeEvent(input$confirmedGroupList, {
#   variables$groupList <- list
#   groupList <- lapply(1:input$groupNum, function(x){
#     input[[paste0("group", x)]]
#   })
#   showNotification("Group information has been update.", type = "message")
#   variables$groupList <- groupList
# })

observeEvent(input$confirmedGroupList, {
  if (nrow(variables$CountData) == 0) {
    showNotification("Please input count data table!", type = "error")
    return()
  }
  if (input$groupSelectViaText == "") {
    showNotification("Please input group information!", type = "error")
    return()
  }

  tryCatch(
    {
      group <- fread(input$groupSelectViaText)
      variables$groupList <-
        lapply(unique(group$V2), function(x) {
          group[group$V2 == x,]$V1
        })
      
      showNotification("Group information has been update.", type = "message")
      showNotification("Generate TCC Parameters.", type = "message")
      
      output$TCC <- renderUI({
        tagList(
          selectInput(
            "normMethod",
            "Normalization method:",
            c("TMM" = "tmm",
              "DESeq" = "deseq")
          ),
          selectInput(
            "testMethod",
            "DEGs identify method:",
            c(
              "edgeR" = "edger",
              "DESeq" = "deseq",
              "DESeq2" = "deseq2",
              "baySeq" = "bayseq",
              "SAMSeq" = "samseq",
              "Voom" = "voom",
              "WAD" = "wad"
            )
          ),
          numericInput(
            "filterLowCount",
            "Filter low count genes threshold:",
            value = 0,
            min = 0
          ),
          sliderInput(
            "iteration",
            "Interation:",
            min = 1,
            max = 50,
            value = 3
          ),
          sliderInput(
            "fdr",
            "FDR:",
            min = 0,
            max = 1,
            value = 0.1,
            step = 0.05
          ),
          sliderInput(
            "floorpdeg",
            "Elimination of Potential DEGs:",
            min = 0,
            max = 1,
            value = 0.05,
            step = 0.05
          ),
          fluidRow(column(6, actionButton("TCC", "Run TCC")),
                   column(6, uiOutput("runTCCCode")))
        )
      })
    },
    error = function(e) {
      showNotification("Check your group information format!", type = "error")
      return()
    },
    warning = function(w) {
      showNotification("Check your group information format!", type = "error")
      return()
    }
  )
})
  
  # validate(
  #   need(input$groupSelectViaText != "", "Please input group information!")
  # )
  # group <- fread(input$groupSelectViaText)
  # variables$groupList <- lapply(unique(group()$V2), function(x) {group()[group()$V2 == x, ]$V1})
  # showNotification("Group information has been update.", type = "message")
# })

# Generate TCC Parameters
# observeEvent(input$confirmedGroupList, {
#   showNotification("Generate TCC Parameters.", type = "message")
#   output$TCC <- renderUI({
#     tagList(
#       selectInput("normMethod", "Normalization method:",
#                 c("TMM" = "tmm",
#                   "DESeq" = "deseq")),
#       selectInput("testMethod", "DEGs identify method:",
#                 c("edgeR" = "edger",
#                   "DESeq" = "deseq",
#                   "DESeq2" = "deseq2",
#                   "baySeq" = "bayseq",
#                   "SAMSeq" = "samseq",
#                   "Voom" = "voom",
#                   "WAD" = "wad")),
#       numericInput("filterLowCount", "Filter low count genes threshold:", value = 0, min = 0),
#       sliderInput("iteration", "Interation:", min = 1, max = 50, value = 3),
#       sliderInput("fdr", "FDR:", min = 0, max = 1, value = 0.1, step = 0.05),
#       sliderInput("floorpdeg", "Elimination of Potential DEGs:", min = 0, max = 1, value = 0.05, step = 0.05),
#       fluidRow(
#         column(6, actionButton("TCC", "Run TCC")),
#         column(6, uiOutput("runTCCCode"))
#       )
#     )
#   })
# })

# 2018-6-16 Unused
# output$groupSelect <- renderUI({
#   if(!is.null(input$groupNum)){
#     tagList(
#       lapply(1:input$groupNum, function(i) {
#         uiOutput(paste0("group", i))
#       })
#     )
#   }
# })

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