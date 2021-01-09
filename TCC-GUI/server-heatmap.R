# server-heatmap.R

runHeatmap <- reactiveValues(runHeatmapValue = FALSE, height = 300)

# Generate heatmap parameters panel ---------------------------------------


observeEvent(input$sider, {
  if (input$sider == "heatmapTab") {
    output$heatmapParameter <- renderUI({
      tagList(
        radioGroupButtons(
          inputId = "heatmapGeneSelectType",
          label = "Select Genes",
          choices = c(
            "By List" = "By list",
            # "By name",
            "By FDR" = "By FDR"
          ),
          justified = TRUE,
          status = "primary"
        ),

        uiOutput("heatmapSelectGene"),

        radioGroupButtons(
          inputId = "heatmapData",
          label = "Source",
          choices = c(
            "Original" = "o",
            "Normalized" = "n"
          ),
          justified = TRUE,
          status = "primary"
        ),
        selectInput(
          "heatmapDist",
          "Distance Measure",
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
          "Agglomeration Method",
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
        materialSwitch(
          inputId = "heatmapLogTrans",
          label = "log(1+x) transform",
          value = TRUE,
          right = TRUE,
          status = "primary"
        ),
        materialSwitch(
          inputId = "heatmapNor",
          label = "Normalization",
          value = FALSE,
          right = TRUE,
          status = "primary"
        ),
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
        radioGroupButtons(
          inputId = "heatmapSwap",
          label = "Direction",
          choices = list(
            "Horizontal" = "h",
            "Vertical" = "v"
          ),
          justified = TRUE,
          status = "primary"
        ),
        selectInput(
          inputId = "colorSelectionMethod",
          label = "Color Selection Method",
          choices = c("Color map", "Two colors", "Three colors")
        ),
        uiOutput("heatmapColorSelectionPanel"),
        sliderInput(
          "heatmapColorNumber",
          "Select the number of colors to be in the palette",
          min = 1,
          max = 50,
          step = 1,
          value = 20
        ),
        tags$b("Color Preview"),
        plotOutput("colorPreview", height = "20px"),
        numericInput(
          inputId = "heatmapHeight",
          label = "Height of Heatmap",
          value = 500,
          min = 100
        ),
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

# Preview gene count -----
observeEvent(input$heatmapFDR, {
  gene_count <-
    nrow(resultTable()[resultTable()$q.value <= input$heatmapFDR, ])
  output$heatmapGeneCountPreview <- renderText({
    paste0(
      "Gene number: ",
      gene_count,
      " | Generation time: ~",
      round(gene_count / 30, 2),
      "s"
    )
  })
})

# According to color selection method, render color selection part --------


observeEvent(input$colorSelectionMethod, {
  if (input$colorSelectionMethod == "Color map") {
    output$heatmapColorSelectionPanel <- renderUI({
      tagList(
        selectInput(
          "heatmapColor",
          "Choose Colormap",
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
        )
      )
    })
  }
  if (input$colorSelectionMethod %in% c("Two colors", "Three colors")) {
    output$heatmapColorSelectionPanel <- renderUI({
      tagList(
        spectrumInput(
          inputId = "heatmapTwoColorLow",
          label = "Low",
          choices = list(
            list(
              "blue",
              "black",
              "white",
              "blanchedalmond",
              "steelblue",
              "forestgreen"
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
            label = "Middle",
            choices = list(
              list(
                "white",
                "black",
                "blanchedalmond",
                "steelblue",
                "forestgreen"
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
          label = "High",
          choices = list(
            list(
              "red",
              "black",
              "white",
              "blanchedalmond",
              "steelblue",
              "forestgreen"
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

# Color palette reactive value --------------------------------------------


colorPanel <- reactive({
  colorPal <- c("white")
  # Create color palette
  if (input$colorSelectionMethod == "Color map" &&
    length(input$heatmapColor) > 0) {
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
  if (input$colorSelectionMethod == "Two colors" &&
    length(input$heatmapTwoColorLow) > 0) {
    colorPal <-
      colorRampPalette(c(input$heatmapTwoColorLow, input$heatmapTwoColorHigh))(input$heatmapColorNumber)
  }
  if (input$colorSelectionMethod == "Three colors" &&
    length(input$heatmapTwoColorLow) > 0) {
    colorPal <-
      colorRampPalette(
        c(
          input$heatmapTwoColorLow,
          input$heatmapTwoColorMiddle,
          input$heatmapTwoColorHigh
        )
      )(input$heatmapColorNumber)
  }

  colorPal
})

# Render a plot of color preview ------------------------------------------


output$colorPreview <- renderPlot({
  colorPal <- colorPanel()
  op <- par(mar = c(0.5, 0, 0, 0))
  plot(
    c(0, length(colorPal)),
    c(0, 1),
    type = "n",
    xlab = "",
    ylab = "",
    ann = F,
    bty = "n",
    xaxt = "n",
    yaxt = "n"
  )
  i <- 0:(length(colorPal) - 1)
  rect(0 + i, 0, 1 + i, 1, col = colorPal, lwd = 0)
  par(op)
})

# Render gene list selection part in parameters panel ---------------------


output$heatmapSelectGene <- renderUI({
  switch(
    input$heatmapGeneSelectType,
    "By list" = textAreaInput(
      "heatmapTextList",
      "Paste Gene List",
      rows = 5,
      placeholder = "Input gene's name (first column in the dataset), one gene per line."
    ),
    # "By name" = selectInput(
    #   "heatmapSelectList",
    #   "Select genes by name",
    #   choices = row.names(variables$CountData),
    #   multiple = TRUE
    # ),
    "By FDR" = tagList(if (input$testMethod != "wad") {
      tagList(
        sliderInput(
          "heatmapFDR",
          "FDR Cut-off",
          min = 0.01,
          max = 1,
          value = 0.01
        ),
        textOutput("heatmapGeneCountPreview")
      )
    } else {
      tagList(
        tags$p("No FDR value for WAD method, please select genes by rank."),
        numericInput("heatmapFDRTop", "Number of top-ranked genes", value = 50)
      )
    })
  )
})


# Create heatmaply object and DataTable object ----------------------------


observeEvent(input$heatmapRun, {
  progressSweetAlert(
    session = session,
    id = "heatmapProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )
  updateProgressBar(
    session = session,
    id = "heatmapProgress",
    title = "Processing data",
    value = 10
  )
  # Select Sample (Column)
  # Grouping.
  data.cl <- variables$groupListConvert

  # Using Original Dataset or Normalized Dataset.
  if (input$heatmapData == "o") {
    data <- variables$CountData[data.cl != 0]
  } else {
    data <- variables$norData
  }
  data.cl <- data.cl[data.cl != 0]

  # Select DEGs (Row)
  tryCatch(
    {
      if (input$heatmapGeneSelectType == "By list") {
        selectedListForHeatmap <-
          row.names(data) %in% unlist(strsplit(x = input$heatmapTextList, split = "[\r\n]"))
        heatmapTitle <- "Heatmap of specific genes"
      }

      if (input$heatmapGeneSelectType == "By FDR") {
        if (input$testMethod == "wad") {
          selectedListForHeatmap <-
            row.names(data) %in% resultTable()[resultTable()$rank <= input$heatmapFDRTop, ]$gene_id

          heatmapTitle <- "Heatmap of specific genes"
        } else {
          selectedListForHeatmap <-
            row.names(data) %in% resultTable()[resultTable()$q.value <= input$heatmapFDR, ]$gene_id

          heatmapTitle <-
            paste0(
              "Heatmap of gene expression (q.value < ",
              input$heatmapFDR,
              ", ",
              sum(selectedListForHeatmap),
              "DEGs)"
            )
        }
      }

      data <- data[selectedListForHeatmap, ]

      if (nrow(data) == 0) {
        sendSweetAlert(
          session = session,
          title = "ERROR",
          text = "Genes list is empty!",
          type = "error"
        )
        return()
      } else {
        showNotification(paste0(dim(data)[1], " DEGs, ", dim(data)[2], " sample will be used."))
        showNotification("Generating, please be patient...", type = "message")
      }

      colorPal <- colorPanel()

      dataBackup <- t(data)

      updateProgressBar(
        session = session,
        id = "heatmapProgress",
        title = "Converting data",
        value = 30
      )
      # Create Plotly object
      output$heatmap <- renderPlotly({
        isolate({
          runHeatmap$height <- input$heatmapHeight
          # Log transform and normalization
          if (input$heatmapLogTrans == TRUE) {
            dataBackup <- log1p(dataBackup)
          }
          if (input$heatmapNor == TRUE) {
            dataBackup <- heatmaply::normalize(dataBackup)
          }

          # This part is for code output ----


          value <- as.character(data.frame(round(dataBackup, 2)))
          sample_name <- row.names(dataBackup)
          gene_name <- colnames(dataBackup)

          value_row <-
            strsplit(x = gsub("c\\(", "", value), split = "(\\), )|(\\))")
          value_row <-
            data.frame(matrix(unlist(value_row)), row.names = gene_name)
          data <-
            data.frame(value_row) %>% separate(colnames(value_row), sample_name, ",")
          data <-
            data.frame(lapply(data, as.numeric), row.names = gene_name)

          code <- paste0(
            "# Your values of data\n",
            "value <- ",
            paste0(
              "c(",
              paste0("'", value, "'", collapse = ",\n"),
              ")",
              collapse = ""
            ),
            "\n\n# Your gene name\n",
            "gene_name <- ",
            paste0(
              "c(",
              paste0("'", gene_name, "'", collapse = ","),
              ")",
              collapse = ""
            ),
            "\n\n# Your sample name\n",
            "sample_name <- ",
            paste0(
              "c(",
              paste0("'", sample_name, "'", collapse = ","),
              ")",
              collapse = ""
            ),
            "\n\n# Convert string to data.frame\n",
            "library(tidyr)",
            '\nvalue_row <- strsplit(x = gsub("c\\\\(", "", value), split = "(\\\\), )|(\\\\))")',
            "\nvalue_row <- data.frame(matrix(unlist(value_row)), row.names = gene_name)",
            '\ndata <- data.frame(value_row) %>% separate(colnames(value_row), sample_name, ",")',
            "\ndata <- data.frame(lapply(data,as.numeric), row.names = gene_name)",
            "\n\n# Generate heatmap\n",
            "library(heatmaply)\n"
          )
          if (input$heatmapSwap == "h") {
            output$heatmapRcode <- renderText({
              codeHeatmap <- paste0(
                "heatmaply(t(data), k_row = ",
                length(variables$groupList),
                ", color = ",
                paste0(
                  "c(",
                  paste0("'", colorPal, "'", collapse = ","),
                  ")",
                  collapse = ""
                ),
                ", dist_method = '",
                input$heatmapDist,
                "', hclust_method = '",
                input$heatmapCluster,
                "', xlab = 'Sample', ylab = 'Gene', scale = '",
                input$heatmapScale,
                "', labRow = colnames(data), labCol = row.names(data))",
                collapse = ""
              )
              paste0(code, codeHeatmap, collapse = "")
            })
            updateProgressBar(
              session = session,
              id = "heatmapProgress",
              title = "Ploting",
              value = 70
            )
            p <- heatmaply(
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
            ) %>%
              config(
                toImageButtonOptions = list(
                  format = "svg",
                  filename = heatmapTitle
                )
              )

            variables$heatmapObject <- p
            p
          } else {
            output$heatmapRcode <- renderText({
              codeHeatmap <- paste0(
                "heatmaply(data, k_row = ",
                length(variables$groupList),
                ", color = ",
                paste0(
                  "c(",
                  paste0("'", colorPal, "'", collapse = ","),
                  ")",
                  collapse = ""
                ),
                ", dist_method = '",
                input$heatmapDist,
                "', hclust_method = '",
                input$heatmapCluster,
                "', xlab = 'Gene', ylab = 'Sample', scale = '",
                input$heatmapScale,
                "', labCol = colnames(data), labRow = row.names(data))",
                collapse = ""
              )
              paste0(code, codeHeatmap, collapse = "")
            })
            updateProgressBar(
              session = session,
              id = "heatmapProgress",
              title = "Ploting",
              value = 70
            )
            p <- heatmaply(
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
            ) %>%
              config(
                toImageButtonOptions = list(
                  format = "svg",
                  filename = heatmapTitle
                )
              )

            variables$heatmapObject <- p
            p
          }
        })
      })

      updateProgressBar(
        session = session,
        id = "heatmapProgress",
        title = "Extracting data",
        value = 90
      )
      # Generate Result table
      output$resultTableInHeatmap <- DT::renderDataTable({
        # Combine TCC Result and Raw Count Data
        gene_id <- row.names(data)
        data <- cbind(data, gene_id = gene_id)

        resultTable <- merge(variables$result, data, by = "gene_id")

        DT::datatable(
          resultTable,
          option = list(
            scrollX = TRUE,
            pageLength = 10,
            searchHighlight = TRUE,
            orderClasses = TRUE
          )
        ) %>%
          formatRound(
            columns = c(
              "a.value",
              "m.value",
              "p.value",
              "q.value",
              colnames(data)[colnames(data) != "gene_id"]
            ),
            digits = 3
          ) %>%
          formatStyle(
            "estimatedDEG",
            target = "row",
            backgroundColor = styleEqual(1, "lightblue")
          )
      })

      updateProgressBar(
        session = session,
        id = "heatmapProgress",
        title = "All done",
        value = 100
      )

      runHeatmap$runHeatmapValue <- input$heatmapRun

      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = "Completed!",
        type = "success"
      )
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
      return()
    }
  )
})

# Render interactive heatmap plot -----------------------------------------


output$heatmapPlot <- renderUI({
  if (runHeatmap$runHeatmapValue) {
    plotlyOutput("heatmap", height = runHeatmap$height) %>% withSpinner()
  }
  else {
    helpText("Click [Generate Heatmap] to plot the heatmap first.")
  }
})
