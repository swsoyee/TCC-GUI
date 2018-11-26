# server-simulation.R

observeEvent(input$simulationGroupNum, {
  lapply(1:input$simulationGroupNum, function(x) {
    output[[paste0("Group", x)]] <- renderUI({
      tagList(column(
        4,
        numericInput(
          inputId = paste0("DEGAssign", x),
          label = paste0("Proportion of DEGs up-regulated"),
          value = 0.5,
          min = 0,
          max = 1
        )
      ),
      column(
        4,
        numericInput(
          inputId = paste0("DEGFoldchange", x),
          label = paste0("Degree of Fold-change"),
          value = 4,
          min = 0
        )
      ),
      column(
        4,
        numericInput(
          inputId = paste0("replicates", x),
          label = paste0("Number of Replicates"),
          value = 3,
          min = 0
        )
      ))
    })
  })
  
  lapply(1:input$simulationGroupNum,
         function(i) {
           outputOptions(output, paste0("Group", i), suspendWhenHidden = FALSE)
         })
})

output$simulationGroup <- renderUI({
  if (input$simulationGroupNum != 0) {
    
    myTabs <- lapply(1:input$simulationGroupNum, function(i) {
      tabPanel(title = paste0("Group", i),
               uiOutput(paste0("Group", i)))
    })
    do.call(tabsetPanel, c(list(id = "simuTab"), myTabs))
  }
})

observeEvent(input$simulationRun, {
  GroupNum <- input$simulationGroupNum
  
  checkAssign <- sum(sapply(1:GroupNum, function(i) {
    input[[paste0("DEGAssign", i)]]
  }))
  if (checkAssign != 1) {
    sendSweetAlert(
      session = session,
      title = "VALUE ERROR",
      text = "Summation of proportion of DEGs assignment (DEG.assign) in all group must be 1",
      type = "error"
    )
  } else {
    DEG.assign <- sapply(1:GroupNum, function(i) {
      input[[paste0("DEGAssign", i)]]
    })
    DEG.foldchange <- sapply(1:GroupNum, function(i) {
      input[[paste0("DEGFoldchange", i)]]
    })
    replicates <- sapply(1:GroupNum, function(i) {
      input[[paste0("replicates", i)]]
    })
    sendSweetAlert(
      session = session,
      title = "RUNING...",
      text = "Generating simulation data, please wait for a moment...",
      type = "info"
    )
    simulatedData <-
      simulateReadCounts(
        Ngene = input$simulationGeneNum,
        PDEG = input$simulationPDEG,
        DEG.assign = DEG.assign,
        DEG.foldchange = DEG.foldchange,
        replicates = replicates
      )
    output$simulatedData <- DT::renderDataTable({
      DT::datatable(
        simulatedData$count,
        option = list(
          scrollX = TRUE,
          pageLength = 10,
          searchHighlight = TRUE,
          orderClasses = TRUE
        )
      )
    })
    
    # Download Simulation Data function
    output$downloadSimuData <- downloadHandler(
      filename = function() {
        paste(
          Sys.Date(),
          "Simulation_data.csv",
          sep = "_"
        )
      },
      content = function(file) {
        write.csv(simulatedData$count, file, row.names = FALSE)
      }
    )
    
    # Render Simulation Data Table and Download Button
    output$simuDataTableAndDownload <- renderUI({
      tagList(
        downloadButton("downloadSimuData", "Download Simulation Data"),
        tags$p("Download this dataset and copy the group infomations, and you can upload it in [Data import (Step1)] tab for analysis"),
        DT::dataTableOutput("simulatedData")
      )
    })
    
    # Store simulation data 
    variables$simulationData <- simulatedData$count
    
    output$simulationGroupInfoText <- renderText({
      paste0(row.names(simulatedData$group),
             ",",
             simulatedData$group$group,
             collapse = "\n")
    })
    
    output$simulationGroupInfo <- renderUI({
      box(
        title = "Group info",
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        verbatimTextOutput("simulationGroupInfoText")
      )
    })
  }
})

output$simuParams <- renderText({
  GroupNum <- input$simulationGroupNum
  DEG.assign <- sapply(1:GroupNum, function(i) {
    input[[paste0("DEGAssign", i)]]
  })
  DEG.foldchange <- sapply(1:GroupNum, function(i) {
    input[[paste0("DEGFoldchange", i)]]
  })
  replicates <- sapply(1:GroupNum, function(i) {
    input[[paste0("replicates", i)]]
  })
  str1 <- paste0("Genes sizes: ", input$simulationGeneNum)
  str2 <- paste0("PDEG: ", input$simulationPDEG)
  str3 <- paste0("DEG.assign: c(", paste(DEG.assign, collapse = ","), ")")
  str4 <- paste0("DEG.Foldchange: c(", paste(DEG.foldchange, collapse = ","), ")")
  str5 <- paste0("Replicates: c(", paste(replicates, collapse = ","), ")")
  paste(str1, str2, str3, str4, str5, sep = "\n")
})
