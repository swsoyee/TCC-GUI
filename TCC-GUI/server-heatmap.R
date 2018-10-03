# server-heatmap.R

# ====================================
# This function render a series UI for heatmap parameters.
# After TCC calculation, generate these UI.
# Position: In Heatmap tab, upper left.
# ====================================

observeEvent(input$TCC, {
  output$heatmapParameter <- renderUI({
    tagList(
      radioButtons(
        "heatmapGeneSelectType",
        "Ways of select gene",
        choices = c(
          "Paste a list of genes",
          "Select genes by name",
          "Select genes according FDR"
        )
      ),
      
      uiOutput("heatmapSelectGene"),
      
      radioButtons(
        "heatmapData",
        "Source:",
        choices = c("Original" = "o",
                    "Normalized" = "n")
      ),
      selectInput(
        "heatmapDist",
        "Distance Method",
        choices = list(
          "Euclidean" = "euclidean",
          "Maximum" = "maximum",
          "Manhattan" = "manhattan",
          "Canberra" = "canberra",
          "Binary" = "binary",
          "Minkowski" = "minkowski"
        ),
        selected = "euclidean"
      ),
      selectInput(
        "heatmapCluster",
        "Hierarchical Clustering Method",
        choices = list(
          "ward.D" = "ward.D",
          "ward.D2" = "ward.D2",
          "Single" = "single",
          "Complete" = "complete",
          "UPGMA" = "average",
          "WPGMA" = "mcquitty",
          "WOGMC" = "median",
          "UPGMC" = "centroid"
        ),
        selected = "complete"
      ),
      selectInput(
        "heatmapScale",
        "Scale",
        choices = list(
          "Row" = "column",
          "Column" = "row",
          "None" = "none"
        )
      ),
      fluidRow(column(6,
        actionButton("heatmapRun", "Run")
      ),
      column(6, uiOutput("runHeatmapCode"))
      )
    )
  })
})

# ====================================
# This function render UI of ways of gene selection.
# Position: In Heatmap tab, upper left.
# ====================================

output$heatmapSelectGene <- renderUI({
  switch(
    input$heatmapGeneSelectType,
    "Paste a list of genes" = textAreaInput(
      "heatmapTextList",
      "Paste a list of genes",
      rows = 5,
      placeholder = "Input gene's name (first column in the dataset), one gene per line."
    ),
    "Select genes by name" = selectInput(
      "heatmapSelectList",
      "Select genes by name",
      choices = row.names(variables$CountData),
      multiple = TRUE
    ),
    "Select genes according FDR" = tagList(
      if (input$testMethod != 'wad') {
        sliderInput(
          "heatmapFDR",
          "FDR:",
          min = 0.01,
          max = 1,
          value = 0.05
        )
      },
      numericInput("heatmapFDRTop", "Top gene count:", value = 50)
    )
  )
})

# ====================================
# This function render Heatmap Plot and result table.
# Position: In Heatmap tab, upper middle.
# ====================================

observeEvent(input$heatmapRun, {
  req(input$heatmapRun)
  isolate({
    # Select Sample (Column)
    # Grouping.
    data.cl<- variables$groupListConvert
    
    # Using Original Dataset or Normalized Dataset.
    if (input$heatmapData == "o") {
      data <- variables$CountData[data.cl != 0]
    } else {
      data <- variables$norData
    }
    data.cl <- data.cl[data.cl != 0]
    
    # Select DEGs (Row)
    tryCatch({
    if (input$heatmapGeneSelectType == "Paste a list of genes") {
      data <-
        data[row.names(data) %in% unlist(strsplit(x = input$heatmapTextList, split = '[\r\n]')), ]
    }
    if (input$heatmapGeneSelectType == "Select genes by name") {
      data <- data[row.names(data) %in% input$heatmapSelectList, ]
    }
    if (input$heatmapGeneSelectType == "Select genes according FDR") {
      if (input$testMethod == 'wad') {
        data <-
          data[row.names(data) %in% resultTable()[resultTable()$rank <= input$heatmapFDRTop,]$gene_id,]
      } else {
        data <-
          data[row.names(data) %in% resultTable()[resultTable()$rank <= input$heatmapFDRTop &
                                                    resultTable()$q.value <= input$heatmapFDR,]$gene_id,]
      }
    }
    
      if(nrow(data) == 0) {
        showNotification("Genes list is empty!", type = "error")
        return()
      } else {
        showNotification(paste0(dim(data)[1], " DEGs, ", dim(data)[2], " sample will be used."))
        showNotification("Generating, please be patient...", type = "message")
      }
    # Create Plotly object
    withBars(output$heatmap <- renderPlotly({
      heatmaply(
        t(data),
        k_row = length(variables$groupList),
        colors = RdYlGn,
        dist_method = input$heatmapDist,
        hclust_method = input$heatmapCluster,
        xlab = "Gene",
        ylab = "Sample",
        main = paste0(
          "Heatmap of gene expression (FDR < ",
          input$heatmapFDR,
          ", ",
          dim(data)[1],
          "DEGs)"
        ),
        margins = c(150, 100, 40, 20),
        scale = input$heatmapScale,
        labCol = colnames(t(data)),
        labRow = row.names(t(data))
      )
    }))
    
    # Generate Result table
    output$resultTableInHeatmap <- DT::renderDataTable({
      # Combine TCC Result and Raw Count Data
      gene_id<-row.names(data)
      data<-cbind(data, gene_id = gene_id)
      
      resultTable <- merge(variables$result, data, by = "gene_id")
      
      DT::datatable(
        resultTable,
        option = list(
          scrollX = TRUE,
          pageLength = 10,
          searchHighlight = TRUE,
          orderClasses = TRUE
        )
      ) %>% formatRound(
        columns = c("a.value",
                    "m.value",
                    "p.value",
                    "q.value",
                    colnames(data)),
        digits = 3
      )
    })
  },
  error = function(e) {
    showNotification("Genes list incorrect!", type = "error")
    return()
  },
  warning = function(w) {
    showNotification("Genes list incorrect!", type = "error")
    return()
  })
  
  # ====================================
  # This function render a button of showing the Heatmap code.
  #
  # Position: In Heatmap tab, under right, in Heatmap Parameters panel.
  # ====================================
  output$runHeatmapCode <- renderUI({
    actionButton("showHeatmapCode", "Show R code")
  })
})
})

# ====================================
# This function check the `Show R code` button, if the botton is clicked,
# show the TCC running code.
# Position: In Computation tab, upper right, in TCC Parameters panel.
# ====================================

observeEvent(input$showHeatmapCode, {
  shinyalert(
    title = "TCC Run code",
    text = variables$runHeatmap,
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

