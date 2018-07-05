# server-heatmap.R

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
      actionButton("heatmapRun", "Run")
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

observeEvent(input$heatmapRun, {
  req(input$heatmapRun)
  isolate({
    # Select Sample (Column)
    # Grouping.
    data.cl <- rep(0, ncol(variables$CountData))
    
    for (i in 1:length(variables$groupList)) {
      data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = variables$CountData))] = i
    }
    # Using Original Dataset or Normalized Dataset.
    if (input$heatmapData == "o") {
      data <- variables$CountData[data.cl != 0]
    } else {
      data <- variables$norData
    }
    data.cl <- data.cl[data.cl != 0]
    
    # Select DEGs (Row)
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
    
    showNotification(paste0(dim(data)[1], " DEGs, ", dim(data)[2], " sample will be used."))
    showNotification("Generating, please be patient...", type = "message")
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
  })
})
