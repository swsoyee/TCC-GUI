# server-data-import.R

# ====================================
# If Sample Data button has been clicked, load sample data. 
# Position: In Computation tab, upper left. Button action.
# ====================================
# 2018-5-23 Change read.table to fread.

observeEvent(input$CountDataSample, {
  variables$CountData <-
    data.frame(fread(input$SampleDatabase), row.names = 1)
  
  sendSweetAlert(
    session = session,
    title = "DONE",
    text = "Count data are successfully loaded.",
    type = "success"
  )
  
  sampleGroup <- switch(input$SampleDatabase,
                        "sample_data/data_hypodata_3vs3.txt" = paste(
                          "G1_rep1,Group1",
                          "G1_rep2,Group1",
                          "G1_rep3,Group1",
                          "G2_rep1,Group2",
                          "G2_rep2,Group2",
                          "G2_rep3,Group2",
                          sep = '\n'
                        ),
                        "sample_data/katzmouse_count_table.txt" = paste(
                          "SRX026633,1",
                          "SRX026632,2",
                          "SRX026631,1",
                          "SRX026630,2",
                          sep = '\n'
                        ),
                        "sample_data/cheung_count_table.txt" = paste(
                          "NA06985,1",
                          "NA07000,1",
                          "NA07055,1",
                          "NA07056,1",
                          "NA07345,1",
                          "NA11830,1",
                          "NA11832,1",
                          "NA11882,1",
                          "NA11993,1",
                          "NA12004,1",
                          "NA12006,1",
                          "NA12044,1",
                          "NA12057,1",
                          "NA12145,1",
                          "NA12717,1",
                          "NA12813,1",
                          "NA12815,1",
                          "NA06993,2",
                          "NA06994,2",
                          "NA07022,2",
                          "NA07034,2",
                          "NA11829,2",
                          "NA11831,2",
                          "NA11839,2",
                          "NA11881,2",
                          "NA11992,2",
                          "NA11994,2",
                          "NA12003,2",
                          "NA12005,2",
                          "NA12043,2",
                          "NA12056,2",
                          "NA12144,2",
                          "NA12155,2",
                          "NA12264,2",
                          "NA12716,2",
                          "NA12750,2",
                          "NA12762,2",
                          "NA12814,2",
                          "NA12872,2",
                          "NA12874,2",
                          "NA12891,2",
                          sep = '\n'
                        ))
  updateTextAreaInput(session, "groupSelectViaText", value = sampleGroup)
  
})


# If Upload Data button has been clicked, load the data via upload ----

observeEvent(input$uploadCountData, {
  showNotification("Start uploading file...", type = "message")
  tryCatch({
    variables$CountData <- data.frame(fread(input$uploadCountData$datapath), row.names=1)
    showNotification("Received uploaded file.", type = "message")
  },
  error = function(e) {
    sendSweetAlert(
      session = session,
      title = "Input data error!",
      text = as.character(message(e)),
      type = "error"
    )
    return()
  },
  warning = function(w) {
    sendSweetAlert(
      session = session,
      title = "Input data warning!",
      text = "Some error is in your dataset, it maybe cause some problem we cannot expected.",
      type = "warning"
    )
    return()
  })
})

datasetInput <- reactive({
  variables$CountData
})


# Render a table of raw count data, adding color ----

output$table <- DT::renderDataTable({
  df <- datasetInput()
  # Create 19 breaks and 20 rgb color values ranging from white to red
  brks <-
    quantile(df %>% select_if(is.numeric),
             probs = seq(.05, .95, .05),
             na.rm = TRUE)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  {
    paste0("rgb(255,", ., ",", ., ")")
  }
  
  DT::datatable(
    df,
    option = list(
      scrollX = TRUE,
      pageLength = 10,
      searchHighlight = TRUE,
      orderClasses = TRUE
    )
  ) %>%
    formatStyle(names(df %>% select_if(is.numeric)), backgroundColor = styleInterval(brks, clrs))
})


# Render DataTable of row data count ----

output$emptyTable <- renderUI({
  if (nrow(datasetInput()) == 0) {
    return("No data to show. Click [Import Data] or [Upload] your own dataset.")
  } else {
    DT::dataTableOutput('table')
  }
})

observeEvent(input$confirmedGroupList, {
  
  if (nrow(variables$CountData) == 0) {
    sendSweetAlert(
      session = session,
      title = "Data error!",
      text = "Please input count data table!",
      type = "error"
    )
    return()
  }
  if (input$groupSelectViaText == "") {
    sendSweetAlert(
      session = session,
      title = "Group error!",
      text = "Please input group information!",
      type = "error"
    )
    return()
  }

  tryCatch(
    {
      progressSweetAlert(
        session = session,
        id = "dataImportProgress",
        title = "Processing group info",
        display_pct = TRUE,
        value = 0
      )
      group <- fread(input$groupSelectViaText, header = FALSE)
      variables$groupList <-
        lapply(unique(group$V2), function(x) {
          group[group$V2 == x,]$V1
        })
      names(variables$groupList) <- unique(group$V2)
      
      data.cl <- rep(0, ncol(variables$CountData))
      
      for (i in 1:length(variables$groupList)) {
        data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = variables$CountData))] = names(variables$groupList[i])
      }
      
      # Storage convert group list to local
      variables$groupListConvert <- data.cl
      
      updateProgressBar(
        session = session,
        id = "dataImportProgress",
        title = "Ploting Sample Distribution",
        value = 20
      )

      
      # This function render a boxplot of sample distribution ----
      
      data <- variables$CountData[variables$groupListConvert != 0]
      cpm <- log2(data / 1000000)
      cpm_stack <- stack(cpm)
      # Add a group column in case of bugs.
      cpm_stack$group <- 0
      # Add Group info
      for (i in 1:length(variables$groupList)) {
        cpm_stack[is.element(cpm_stack$ind, variables$groupList[[i]]), ]$group <-
          names(variables$groupList[i])
      }
      # Reorder X axis, in case of some those dataset which sample name are not 
      # in group order
      cpm_stack_order <-
        unique(cpm_stack[order(cpm_stack$group), ]$ind)
      xform <- list(
        categoryorder = "array",
        categoryarray = cpm_stack_order
      )
      
      updateProgressBar(
        session = session,
        id = "dataImportProgress",
        title = "Ploting Sample Distribution",
        value = 40
      )
      
      psd <- plot_ly(
        data = cpm_stack,
        x =  ~ ind,
        y =  ~ values,
        type = "box",
        split = ~group,
        color = ~group
      )
      withBars(output$sampleDistribution <- renderPlotly({
        xform$title <- input$sampleDistributionXlab
        p <- psd %>%
          layout(
            title = input$sampleDistributionTitle,
            xaxis = xform,
            yaxis = list(title = input$sampleDistributionYlab),
            legend = list(
              orientation = 'h',
              xanchor = "center",
              x = 0.5,
              y = input$sampleDistributionLegendY
            )
          )
        variables$sampleDistributionBar <- p
        p
      }))
      updateProgressBar(
        session = session,
        id = "dataImportProgress",
        title = "Ploting Sample Distribution",
        value = 60
      )
      # The same plot used in Calculation tab.
      # withBars(output$sampleDistributionTCC <- renderPlotly({
      #   xform$title <- input$sampleDistributionXlab
      #   psd %>%
      #     layout(
      #       title = input$sampleDistributionTitle,
      #       xaxis = xform,
      #       yaxis = list(title = input$sampleDistributionYlab),
      #       legend = list(
      #         orientation = 'h',
      #         xanchor = "center",
      #         x = 0.5,
      #         y = input$sampleDistributionLegendY
      #       )
      #     )
      # }))

      # This function render a density plot of sample distribution ----

      updateProgressBar(
        session = session,
        id = "dataImportProgress",
        title = "Ploting Sample Distribution Density",
        value = 80
      )
      
      densityTable <-lapply(cpm, density)
      p <- plot_ly(type = "scatter", mode = "lines")
      for(i in 1:length(densityTable)){
        p <- add_trace(p, x = densityTable[[i]][[1]],
                       y = densityTable[[i]][[2]],
                       # fill = "tozeroy",
                       color = group[V1 == names(densityTable[i]), V2],
                       name = names(densityTable[i]))
      }
      
      withBars(output$sampleDistributionDensity <- renderPlotly({
        pp <- p %>%
          layout(
            title = input$sampleDistributionDenstityTitle,
            xaxis = list(title = input$sampleDistributionDensityXlab),
            yaxis = list(title = input$sampleDistributionDensityYlab),
            legend = list(
              orientation = 'h',
              xanchor = "center",
              x = 0.5,
              y = input$sampleDistributionDensityLegendY
            )
          )
        variables$sampleDistributionDensity <- pp
        pp
      }))
      
      # The same plot used in Calculation tab.
      withBars(output$sampleDistributionDensityTCC <- renderPlotly({
        p %>%
          layout(
            title = input$sampleDistributionDenstityTitle,
            xaxis = list(title = input$sampleDistributionDensityXlab),
            yaxis = list(title = input$sampleDistributionDensityYlab),
            legend = list(
              orientation = 'h',
              xanchor = "center",
              x = 0.5,
              y = input$sampleDistributionDensityLegendY
            )
          )
      }))
      
      # This function render a series infoBox of summary of data ----

      updateProgressBar(
        session = session,
        id = "dataImportProgress",
        title = "Summarizing data",
        value = 90
      )

      # Infobox of row and column number in the row count dataset ---------------
      
      
      output$rowOfCountData <- renderUI({
        infoBox(
          title = "SHAPE",
          value = paste0(
            "c(",
            dim(variables$CountData)[1],
            ", ",
            dim(variables$CountData)[2],
            ")"
          ),
          subtitle = "Row and column numbers in the dataset",
          width = NULL,
          icon = icon("list"),
          fill = TRUE,
          color = "yellow"
        )
      })
      
      # Infobox of group informations -------------------------------------------
      
      
      output$groupCount <- renderUI({
        groupText <- sapply(variables$groupList, length)
        infoBox(
          title = "Group Number", 
          value = length(groupText), 
          subtitle = paste0(names(groupText), ": ",groupText, collapse = "\n"),
          width = NULL,
          icon = icon("users"),
          fill = TRUE,
          color = "aqua"
        )
      })
      
      # Infobox of zero expression in all sample --------------------------------
      output$zeroValue <- renderUI({
        zeroValue <-
          sum((apply(variables$CountData, 1, function(x) {
            sum(x) == 0
          })))
        variables$zeroValue <- paste0(zeroValue, 
                                      " (", 
                                      round(zeroValue / nrow(variables$CountData) * 100, 2), 
                                      "%)")
        infoBox(
          title = "NON-EXPRESSED",
          value = variables$zeroValue,
          subtitle = "Number of genes with zero counts across samples",
          width = NULL,
          icon = icon("exclamation-circle"),
          fill = TRUE,
          color = "olive"
        )
      })
      
      # Infobox of Silhouette Score ----
      output$silhouette <- renderUI({
        data <- variables$CountData
        data.cl <- variables$groupListConvert
        
        data.cl <- data.cl[data.cl != 0]
        data <- data[data.cl != 0]
        
        # Filtering
        obj <- as.logical(rowSums(data) > 0)
        data <- unique(data[obj,])
        
        # AS calculation
        d <- as.dist(1 - cor(data, method="spearman"))
        AS <-  mean(silhouette(rank(data.cl, ties.method = "min"), d)[, "sil_width"])

        infoBox(title = "average Silhouette",
                value = round(AS, 3),
                subtitle = "Degrees of separation between different groups",
                width = NULL,
                icon = icon("chain"),
                fill = TRUE,
                href = "https://biologicalproceduresonline.biomedcentral.com/articles/10.1186/s12575-018-0067-8",
                color = "purple")
      })
      
      closeSweetAlert(session = session)
      sendSweetAlert(session = session,
                     title = "DONE",
                     text = "Group labels are successfully assigned.",
                     type = "success")
    },
    error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Group error!",
        text = "Check your group information format!",
        type = "error"
      )
      return()
    },
    warning = function(w) {
      sendSweetAlert(
        session = session,
        title = "Group error!",
        text = "Check your group information format!",
        type = "error"
      )
      return()
    }
  )
})




