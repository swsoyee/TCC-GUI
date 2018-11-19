# server-heatmap.R

# ====================================
# This function render a series UI for heatmap parameters.
# After TCC calculation, generate these UI.
# Position: In Heatmap tab, upper left.
# ====================================

observeEvent(input$sider, {
  if (input$sider == "heatmapTab") {
    output$heatmapParameter <- renderUI({
      tagList(
        radioGroupButtons(
          inputId = "heatmapGeneSelectType",
          label = "Select gene",
          choices = c(
            "By list",
            # "By name",
            "By FDR"
          ),
          justified = TRUE,
          status = "primary"
        ),
        
        uiOutput("heatmapSelectGene"),
        
        radioGroupButtons(
          inputId = "heatmapData",
          label = "Source:",
          choices = c("Original" = "o",
                      "Normalized" = "n"),
          justified = TRUE,
          status = "primary"
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
        materialSwitch(inputId = "heatmapLogTrans", label = "log(1+x) transform", value = TRUE, right = TRUE, status = "primary"),
        materialSwitch(inputId = "heatmapNor", label = "Normalization", value = FALSE, right = TRUE, status = "primary"),
        radioGroupButtons(
          inputId = "heatmapScale",
          label = "Scale",
          choices = list(
            "None" = "none",
            "Row" = "row",
            "Column" = "column"
          ), 
          justified = TRUE,
          status = "primary"
        ),
        radioGroupButtons(inputId = "heatmapSwap", 
                          label = "Direction",
                          choices = list("Horizontal" = "h",
                                         "Vertical" = "v"),
                          justified = TRUE,
                          status = "primary"),
        selectInput(inputId = "colorSelectionMethod", 
                    label = "Color selection method",
                    choices = c("Color map", "Two colors", "Three colors")),
        uiOutput("heatmapColorSelectionPanel"),
        # selectInput("heatmapColor", "Choose colormap",
        #             choices = list("PiYG",
        #                            "PRGn",
        #                            "BrBG",
        #                            "PuOr",
        #                            "RdGy",
        #                            "RdBu",
        #                            "RdYlBu",
        #                            "RdYlGn",
        #                            "Spectral",
        #                            "coolwarm"),
        #             selected = "RdYlGn"),
        sliderInput("heatmapColorNumber", "Select the number of colors to be in the palette", min = 1, max = 50, step = 1, value = 20),
        do.call(actionBttn, c(
          list(
            inputId = "heatmapRun",
            label = "Run Heatmap",
            icon = icon("play")
          ),
          actionBttnParams
        ))
      )
    })
  }
})

observeEvent(input$colorSelectionMethod, {
  if (input$colorSelectionMethod == "Color map") {
    output$heatmapColorSelectionPanel <- renderUI({
      tagList(
        selectInput(
          "heatmapColor",
          "Choose colormap",
          choices = list(
            "PiYG",
            "PRGn",
            "BrBG",
            "PuOr",
            "OrRd",
            "Oranges",
            "RdGy",
            "RdBu",
            "RdYlBu",
            "RdYlGn",
            "Spectral",
            "coolwarm"
          ),
          selected = "RdYlGn"
        )#,
        # sliderInput(
        #   "heatmapColorNumber",
        #   "Select the number of colors to be in the palette",
        #   min = 1,
        #   max = 50,
        #   step = 1,
        #   value = 20
        # )
      )
    })
  }
  if (input$colorSelectionMethod %in% c("Two colors", "Three colors")) {
    output$heatmapColorSelectionPanel <- renderUI({
      tagList(
        spectrumInput(
          inputId = "heatmapTwoColorLow",
          label = "Low:",
          choices = list(
            list(
              "#FEFCDB",
              'black',
              'white',
              'blanchedalmond',
              'steelblue',
              'forestgreen'
            ),
            as.list(brewer.pal(n = 9, name = "Blues")),
            as.list(brewer.pal(n = 9, name = "Greens")),
            as.list(brewer.pal(n = 11, name = "Spectral")),
            as.list(brewer.pal(n = 8, name = "Dark2"))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        ),
        if (input$colorSelectionMethod == "Three colors") {
          spectrumInput(
            inputId = "heatmapTwoColorMiddle",
            label = "Middle:",
            choices = list(
              list(
                "#B22222",
                'black',
                'white',
                'blanchedalmond',
                'steelblue',
                'forestgreen'
              ),
              as.list(brewer.pal(n = 9, name = "Blues")),
              as.list(brewer.pal(n = 9, name = "Greens")),
              as.list(brewer.pal(n = 11, name = "Spectral")),
              as.list(brewer.pal(n = 8, name = "Dark2"))
            ),
            options = list(`toggle-palette-more-text` = "Show more")
          )
        },
        spectrumInput(
          inputId = "heatmapTwoColorHigh",
          label = "High:",
          choices = list(
            list(
              "#8C0004",
              'black',
              'white',
              'blanchedalmond',
              'steelblue',
              'forestgreen'
            ),
            as.list(brewer.pal(n = 9, name = "Blues")),
            as.list(brewer.pal(n = 9, name = "Greens")),
            as.list(brewer.pal(n = 11, name = "Spectral")),
            as.list(brewer.pal(n = 8, name = "Dark2"))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        )
      )
    })
  }
})

# ====================================
# This function render UI of ways of gene selection.
# Position: In Heatmap tab, upper left.
# ====================================

output$heatmapSelectGene <- renderUI({
  switch(
    input$heatmapGeneSelectType,
    "By list" = textAreaInput(
      "heatmapTextList",
      "Paste a list of genes",
      rows = 5,
      placeholder = "Input gene's name (first column in the dataset), one gene per line."
    ),
    # "By name" = selectInput(
    #   "heatmapSelectList",
    #   "Select genes by name",
    #   choices = row.names(variables$CountData),
    #   multiple = TRUE
    # ),
    "By FDR" = tagList(
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
    if (input$heatmapGeneSelectType == "By list") {
      data <-
        data[row.names(data) %in% unlist(strsplit(x = input$heatmapTextList, split = '[\r\n]')), ]
      heatmapTitle <- "Heatmap of specific genes"
    }

    if (input$heatmapGeneSelectType == "By FDR") {
      if (input$testMethod == 'wad') {
        data <-
          data[row.names(data) %in% resultTable()[resultTable()$rank <= input$heatmapFDRTop,]$gene_id,]
        heatmapTitle <- "Heatmap of specific genes"
      } else {
        data <-
          data[row.names(data) %in% resultTable()[resultTable()$rank <= input$heatmapFDRTop &
                                                    resultTable()$q.value <= input$heatmapFDR,]$gene_id,]
        heatmapTitle <- paste0(
          "Heatmap of gene expression (FDR < ",
          input$heatmapFDR,
          ", ",
          dim(data)[1],
          "DEGs)"
        )
      }
    }
    
      if(nrow(data) == 0) {
        sendSweetAlert(
          session = session,
          title = "List contents error!",
          text = "Genes list is empty!",
          type = "error"
        )
        return()
      } else {
        showNotification(paste0(dim(data)[1], " DEGs, ", dim(data)[2], " sample will be used."))
        showNotification("Generating, please be patient...", type = "message")
      }
      
    # Create color palette
      if (input$colorSelectionMethod == "Color map") {
        colorPal <- switch(
          input$heatmapColor,
          "PiYG" = PiYG(input$heatmapColorNumber),
          "PRGn" = PRGn(input$heatmapColorNumber),
          "BrBG" = BrBG(input$heatmapColorNumber),
          "PuOr" = PuOr(input$heatmapColorNumber),
          "OrRd" = OrRd(input$heatmapColorNumber),
          "Oranges" = Oranges(input$heatmapColorNumber),
          "RdGy" = RdGy(input$heatmapColorNumber),
          "RdBu" = RdBu(input$heatmapColorNumber),
          "RdYlBu" = RdYlBu(input$heatmapColorNumber),
          "RdYlGn" = RdYlGn(input$heatmapColorNumber),
          "Spectral" = Spectral(input$heatmapColorNumber),
          "coolwarm" = cool_warm(input$heatmapColorNumber)
        )
      }
      if (input$colorSelectionMethod == "Two colors") {
        colorPal <-
          colorRampPalette(c(input$heatmapTwoColorLow, input$heatmapTwoColorHigh))(input$heatmapColorNumber)
      }
      if (input$colorSelectionMethod == "Three colors") {
        colorPal <-
          colorRampPalette(
            c(
              input$heatmapTwoColorLow,
              input$heatmapTwoColorMiddle,
              input$heatmapTwoColorHigh
            )
          )(input$heatmapColorNumber)
      }
    
    dataBackup <- t(data)
    
    # Create Plotly object
    withBars(output$heatmap <- renderPlotly({
      req(input$heatmapRun)
      isolate({
      # Log transform and normalization
      if(input$heatmapLogTrans == TRUE) {
        dataBackup <-  log1p(dataBackup)
      }
      if(input$heatmapNor == TRUE) {
        dataBackup <- heatmaply::normalize(dataBackup)
      }
      
      if(input$heatmapSwap == "h"){
        variables$heatmapHeight <- 10 * ncol(dataBackup)
      heatmaply(
        dataBackup,
        k_row = length(variables$groupList),
        colors = colorPal,
        dist_method = input$heatmapDist,
        hclust_method = input$heatmapCluster,
        xlab = "Gene",
        ylab = "Sample",
        main = heatmapTitle,
        margins = c(150, 100, 40, 20),
        scale = input$heatmapScale,
        labCol = colnames(dataBackup),
        labRow = row.names(dataBackup)
      )} else {
        variables$heatmapHeight <- 20 * ncol(dataBackup)
        heatmaply(
          t(dataBackup),
          k_col = length(variables$groupList),
          colors = colorPal,
          dist_method = input$heatmapDist,
          hclust_method = input$heatmapCluster,
          xlab = "Sample",
          ylab = "Gene",
          main = heatmapTitle,
          margins = c(150, 100, 40, 20),
          scale = input$heatmapScale,
          labCol = row.names(dataBackup),
          labRow = colnames(dataBackup)
        )
      }
      })
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
    sendSweetAlert(
      session = session,
      title = "List contents error!",
      text = "Genes list incorrect!",
      type = "error"
    )
    return()
  },
  warning = function(w) {
    sendSweetAlert(
      session = session,
      title = "List contents error!",
      text = "Genes list incorrect!",
      type = "error"
    )
    # showNotification("Genes list incorrect!", type = "error")
    return()
  })
  
  # ====================================
  # This function render a button of showing the Heatmap code.
  #
  # Position: In Heatmap tab, under right, in Heatmap Parameters panel.
  # ====================================

})
 
output$heatmapPlot <- renderUI({
  withBarsUI(plotlyOutput("heatmap", height = paste0(variables$heatmapHeight, "px")))
})