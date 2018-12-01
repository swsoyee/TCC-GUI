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
            label = HTML("Assignment of DEGs (P<sub>", paste0("G", x), "</sub>)"),
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
            label = HTML("Degree of Fold-change (FC<sub>", paste0("G", x), "</sub>)"),
            value = 4,
            min = 0
          )
        ),
        column(
          4,
          numericInput(
            inputId = paste0("replicates", x),
            label = HTML("Number of Replicates (NR<sub>", paste0("G", x), "</sub>)"),
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

v <- reactiveValues(simulationRunValue = FALSE)

# Excute simulation data ----
observeEvent(input$simulationRun, {
  
  v$simulationRunValue <- input$simulationRun
  
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
    
    if(input$simulationSeed != -1){
      set.seed(input$simulationSeed)
    }
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
    
    # Create 19 breaks and 20 rgb color values ranging from white to red
    df <- data.frame(simulatedData$count)
    brks <-
      quantile(df %>% select_if(is.numeric),
               probs = seq(0.05, 0.95, 0.05),
               na.rm = TRUE)
    
    output$simulatedData <- DT::renderDataTable({
      DT::datatable(
        simulatedData$count,
        colnames = c("Gene Name" = 1),
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
            "Download this dataset and copy the group information, and you can upload them in [Data Import (Step1)] tab for analysis."
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
          scrollX = TRUE,
          scrollY = 400,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          scroller = TRUE
        )
      )%>% formatStyle(names(df %>% select_if(is.numeric)), backgroundColor = styleInterval(brks, head(Blues(40), n = length(brks) + 1)))
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
        write.csv(simulatedData$count, file, row.names = TRUE)
      }
    )
    
    # Store simulation data 
    variables$simulationData <- simulatedData$count
    
    output$simulationGroupInfoText <- renderText({
      paste0(row.names(simulatedData$group),
             ",G",
             simulatedData$group$group,
             collapse = "\n")
    })
    
    output$simulationGroupInfo <- renderUI({
      tagList(
        tags$hr(),
        tags$p(tags$b("Group information:")),
        verbatimTextOutput("simulationGroupInfoText")
      )
    })
    
    updateProgressBar(
      session = session,
      id = "simulationProgress",
      title = "Simulation completed!",
      value = 100
    )
    
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,
                   title = "DONE",
                   text = "Simulation data were successfully generated.",
                   type = "success")
  }
})

# Render Simulation Data Table and Download Button ----
output$simuDataTableAndDownload <- renderUI({
  if (!v$simulationRunValue) {
    tagList(
      tags$p(
        'You haven\'t generated any simulation data. Please click "Generate Simulation Data".'
      )
    )
  } else {
    tagList(fluidRow(column(
      12,
      downloadButton("downloadSimuData", "Download All Simulation Data (CSV)")
    )),
    tags$br(),
    fluidRow(column(
      12, DT::dataTableOutput("simulatedData")
    )))
  }
})


# Render HTML of simulation parameters summary preview ----
output$simuParams <- renderUI({
  GroupNum <- input$simulationGroupNum
  DEG.assign <- sapply(1:GroupNum, function(i) {
    input[[paste0("DEGAssign", i)]]
  })
  names(DEG.assign) <- sapply(1:GroupNum, function(i) {
    paste0("P<sub>G", i, "</sub>")
  })
  DEG.foldchange <- sapply(1:GroupNum, function(i) {
    input[[paste0("DEGFoldchange", i)]]
  })
  names(DEG.foldchange) <- sapply(1:GroupNum, function(i) {
    paste0("FC<sub>G", i, "</sub>")
  })
  replicates <- sapply(1:GroupNum, function(i) {
    input[[paste0("replicates", i)]]
  })
  names(replicates) <- sapply(1:GroupNum, function(i) {
    paste0("NR<sub>G", i, "</sub>")
  })

  tagList(
    tags$div(style = "line-height:100%;",
    HTML("<p><b>Number of Genes ( N<sub>gene</sub> ): </b>", GroupNum, "</p>"),
    HTML("<p><b>Proportion of DEGs ( P<sub>DEG</sub> ): </b>", input$simulationPDEG, "</p>"),
    HTML("<p><b>Number of Groups ( N<sub>group</sub> ): </b>", input$simulationGroupNum, "</p>"),
    HTML("<p><b>Assignment of DEGs (", paste0(names(DEG.assign), collapse = ", "), "): </b>", paste0(DEG.assign, collapse = ", "), "</p>"),
    HTML("<p><b>Degree of Fold-change (", paste0(names(DEG.foldchange), collapse = ", "), "): </b>", paste0(DEG.foldchange, collapse = ", "), "</p>"),
    HTML("<b>Number of Replicates (", paste0(names(replicates), collapse = ", "), "): </b>", paste0(replicates, collapse = ", "), "</p>"),
    uiOutput("simulationGroupInfo")
    )
  )
})
