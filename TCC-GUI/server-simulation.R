# server-simulation.R

# Render input widget in group parameters ----
observeEvent(input$simulationGroupNum, {
  lapply(1:input$simulationGroupNum, function(x) {
    output[[paste0("Group", x)]] <- renderUI({
      tagList(fluidRow(
        column(
          4,
          numericInput(
            inputId = paste0("DEGAssign", x),
            label = paste0("Proportion of assigned DEGs (this group)"),
            value = 0.5,
            step = 0.01,
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
        )
      ),
      fluidRow(column(12, textOutput(paste0(
        "ingroupDEGsNum", x
      )))))
    })
  })
  
  lapply(1:input$simulationGroupNum,
         function(i) {
           outputOptions(output, paste0("Group", i), suspendWhenHidden = FALSE)
         })
})


# According to group number, update proportion ingroup DEGs ----
observeEvent(input$simulationGroupNum, {
  numberLength <- nchar(as.character(input$simulationGeneNum))
  defaultDEG <- round(1 / input$simulationGroupNum, numberLength - 1)
  
  firstDefault <- 1 - (defaultDEG * (input$simulationGroupNum - 1))
  updateNumericInput(session, "DEGAssign1", value = firstDefault)
  
  lapply(2:input$simulationGroupNum, function(x) {
    updateNumericInput(session, paste0("DEGAssign", x), value = defaultDEG)
  })
})

# According all params, preview ingroup DEGs ----
observeEvent({
  input$simulationGeneNum
  input$simulationPDEG
  input$simulationGroupNum
}, {
  lapply(1:input$simulationGroupNum, function(x) {
    output[[paste0("ingroupDEGsNum", x)]] <- renderText({
      paste0(
        "This dataset contains (",
        input$simulationGeneNum,
        " × ",
        input$simulationPDEG,
        " × ",
        input[[paste0("DEGAssign", x)]],
        ") = ",
        round(input$simulationGeneNum * input$simulationPDEG * input[[paste0("DEGAssign", x)]], 0),
        " DEGs up-regulated in this group."
      )
    })
  })
})

# Render Text of DEGs number preview ----
observeEvent(input$simulationPDEG, {
  output$expectedDEGsText <- renderText({
    paste0("Total Number of DEGs: ", floor(input$simulationPDEG * input$simulationGeneNum))
  })
})

# Render a series tab in group parameters ----
output$simulationGroup <- renderUI({
  if (input$simulationGroupNum != 0) {
    
    myTabs <- lapply(1:input$simulationGroupNum, function(i) {
      tabPanel(title = paste0("Group", i),
               uiOutput(paste0("Group", i)))
    })
    do.call(tabsetPanel, c(list(id = "simuTab"), myTabs))
  }
})

# Excute simulation data ----
observeEvent(input$simulationRun, {
  GroupNum <- input$simulationGroupNum
  
  checkAssign <- sum(sapply(1:GroupNum, function(i) {
    input[[paste0("DEGAssign", i)]]
  }))
  if (checkAssign != 1) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Summation of proportion of DEGs assignment (DEG.assign) in all group must be 1",
      type = "error"
    )
  } else {
    progressSweetAlert(
      session = session,
      id = "simulationProgress",
      title = "Loading parameters...",
      display_pct = TRUE,
      value = 0
    )
    
    DEG.assign <- sapply(1:GroupNum, function(i) {
      input[[paste0("DEGAssign", i)]]
    })
    DEG.foldchange <- sapply(1:GroupNum, function(i) {
      input[[paste0("DEGFoldchange", i)]]
    })
    replicates <- sapply(1:GroupNum, function(i) {
      input[[paste0("replicates", i)]]
    })
    updateProgressBar(
      session = session,
      id = "simulationProgress",
      title = "Start generating...",
      value = 20
    )
    simulatedData <-
      simulateReadCounts(
        Ngene = input$simulationGeneNum,
        PDEG = input$simulationPDEG,
        DEG.assign = DEG.assign,
        DEG.foldchange = DEG.foldchange,
        replicates = replicates
      )
    updateProgressBar(
      session = session,
      id = "simulationProgress",
      title = "Finish generating...",
      value = 80
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
    
    # Download Simulation Data function ----
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
    
    # Render Simulation Data Table and Download Button ----
    output$simuDataTableAndDownload <- renderUI({
      tagList(
        downloadButton("downloadSimuData", "Download Simulation Data"),
        tags$p("Download this dataset and copy the group information, and you can upload them in [Data Import (Step1)] tab for analysis."),
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
        title = tagList(icon("info-circle"), "Group information"),
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        verbatimTextOutput("simulationGroupInfoText")
      )
    })
    
    updateProgressBar(
      session = session,
      id = "simulationProgress",
      title = "All done.",
      value = 100
    )
    
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,
                   title = "Simulation completed!",
                   type = "success")
  }
})


# Render Text of simulation parameters summary preview ----
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
  str1 <- paste0("Gene number: ", input$simulationGeneNum)
  str2 <- paste0("PDEG: ", input$simulationPDEG)
  str3 <- paste0("DEG.assign: c(", paste(DEG.assign, collapse = ","), ")")
  str4 <- paste0("DEG.Foldchange: c(", paste(DEG.foldchange, collapse = ","), ")")
  str5 <- paste0("Replicates: c(", paste(replicates, collapse = ","), ")")
  paste(str1, str2, str3, str4, str5, sep = "\n")
})
