# server-data-import.R

# ====================================
# If Sample Data button has been clicked, load sample data. 
# Position: In Computation tab, upper left. Button action.
# ====================================
# 2018-5-23 Change read.table to fread.

observeEvent(input$CountDataSample, {
  variables$tccObject <- NULL
  v$importActionValue <- FALSE
  variables$CountData <-
    data.frame(fread(input$SampleDatabase), row.names = 1)
  
  sendSweetAlert(
    session = session,
    title = "DONE",
    text = "Count data were successfully loaded.",
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
    variables$tccObject <- NULL
    v$importActionValue <- FALSE
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
  
  DT::datatable(
    df,
    colnames = c("Gene Name" = 1),
    extensions = c("Scroller","RowReorder"),
    option = list(
      rowReorder = TRUE,
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE,
      orderClasses = TRUE
    )
  ) %>%
    formatStyle(names(df %>% select_if(is.numeric)), backgroundColor = styleInterval(brks, head(Blues(40), n = length(brks) + 1)))
})


# Render DataTable of row data count ----

output$emptyTable <- renderUI({
  if (nrow(datasetInput()) == 0) {
    helpText("No data to show. Click [Import Data] or [Upload] your own dataset.")
  } else {
    DT::dataTableOutput('table')
  }
})

observeEvent(input$confirmedGroupList, {
  
  if (nrow(datasetInput()) == 0) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Please input count data table!",
      type = "error"
    )
    return()
  }
  if (input$groupSelectViaText == "") {
    sendSweetAlert(
      session = session,
      title = "ERROR",
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
      
      # Create TCC Object
      tcc <- new("TCC", variables$CountData[data.cl != 0], data.cl[data.cl != 0])
      variables$tccObject <- tcc
      
      updateProgressBar(
        session = session,
        id = "dataImportProgress",
        title = "Summarizing data",
        value = 90
      )
      
      closeSweetAlert(session = session)
      sendSweetAlert(session = session,
                     title = "DONE",
                     text = "Group labels are successfully assigned.",
                     type = "success")
      
      v$importActionValue <- input$confirmedGroupList
    },
    error = function(e) {
      sendSweetAlert(
        session = session,
        title = "ERROR",
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

output$importDataSummary <- renderUI({
  dt<- datasetInput()
  
  rowCount <- nrow(dt)
  groupCount <- length(variables$groupList)
  groupText <- sapply(variables$groupList, length)
  if(length(groupText) > 0){
    gText <- paste0(names(groupText), ": ",groupText, collapse = "\n")
  } else {
    gText <- NULL
  }
  
  # AS Part
  data <- variables$CountData
  data.cl <- variables$groupListConvert
  cName <- unlist(variables$groupList)
  
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
    data <- tcc$count
    data.cl <- tcc$group$group
    # Filtering
    obj <- as.logical(rowSums(data) > 0)
    data <- unique(data[obj,])
    
    # AS calculation
    d <- as.dist(1 - cor(data, method="spearman"))
    AS <-  mean(silhouette(rank(data.cl, ties.method = "min"), d)[, "sil_width"])
    AS <- tagList(
      tags$p(tags$b("Average Silhouettes (AS):"), round(AS, 3)),
      HTML(
        'A higher AS value <font color="##00C0EF">(0 ≤ AS ≤ 1)</font> indicates a higher degree of group separation (i.e., a higher percentage of DEG).
        '
      )
    )
  } else {
    AS <- helpText("Assign group information needed.")
  }
  tagList(
    tags$p(tags$b("Number of Genes:"), rowCount),
    tags$p(tags$b("Number of Groups:"), groupCount),
    tags$p(tags$b("Number of Replicates:")),
    tags$p(gText),
    AS
  )
})

v <- reactiveValues(importActionValue = FALSE)

# This function render a boxplot of sample distribution ----

output$sampleDistributionBox <- renderPlotly({
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
    data <- tcc$count

    cpm <- log2(data + 1)
    cpm_stack <- data.frame(stack(cpm))

    # Add Group info
    group <- data.frame("col" = rownames(tcc$group), "group" = tcc$group$group)
    data <- left_join(cpm_stack, group, by = "col")
    data <- arrange(data, group)
    
    p <- plot_ly(
      data = data,
      x = ~ col,
      y = ~ NA..1,
      type = "box",
      split = ~ group,
      color = ~ group
    ) %>% layout(
      title = input$sampleDistributionTitle,
      xaxis = list(title = input$sampleDistributionXlab),
      yaxis = list(title = input$sampleDistributionYlab)
    )
    variables$sampleDistributionBar <- p
    p
  } else {
    return()
  }
})

# Render a density plot of sample distribution ----
output$sampleDistributionDensity <- renderPlotly({
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
    if (input$densityFilter > -1) {
      count <-
        filterLowCountGenes(tcc, low.count = input$densityFilter)$count
    } else {
      count <- tcc$count
    }
    data <- log2(count + 1)
    
    group <- tcc$group
    densityTable <- apply(data, 2, function(x) {
      density(x)
    })
    p <- plot_ly(type = "scatter", mode = "lines")
    for (i in 1:length(densityTable)) {
      p <- add_trace(
        p,
        x = densityTable[[i]][[1]],
        y = densityTable[[i]][[2]],
        color = group[rownames(group) == names(densityTable[i]), ],
        name = names(densityTable[i])
      )
    }
    
    pp <- p %>%
      layout(
        title = input$sampleDistributionDenstityTitle,
        xaxis = list(title = input$sampleDistributionDensityXlab),
        yaxis = list(title = input$sampleDistributionDensityYlab)
      )
    variables$sampleDistributionDensity <- pp
    pp
  } else {
    return()
  }
})

# Sample Distribution Density Plot UI ----
output$sampleDistributionDensityPanel <- renderUI({
  if (v$importActionValue) {
    tagList(fluidRow(
      column(
        3,
        sliderInput(inputId = "densityFilter",
                    label = "Filter genes threshold",
                    min = -1,
                    max = 100,
                    value = -1),
        textInput(
          inputId = "sampleDistributionDenstityTitle",
          label = "Title",
          value = "Original Raw Count",
          placeholder = "Original Raw Count"
        ),
        textInput(
          inputId = "sampleDistributionDensityXlab",
          label = "X label",
          value = "log<sub>2</sub>(Count + 1)",
          placeholder = "log<sub>2</sub>(Count + 1)"
        ),
        textInput(
          inputId = "sampleDistributionDensityYlab",
          label = "Y label",
          value = "Density",
          placeholder = "Density"
        )
      ),
      column(
        9,
        plotlyOutput("sampleDistributionDensity") %>% withSpinner()
      )
    ))
  } else {
    helpText("No data for ploting. Please import dataset and assign group information first.")
  }
})

# Sample Distribution Boxplot UI ----
output$sampleDistributionBoxPanel <- renderUI({
  if (v$importActionValue) {
    tagList(fluidRow(
      column(
        3,
        textInput(
          inputId = "sampleDistributionTitle",
          label = "Title",
          value = "Original Raw Count",
          placeholder = "Original Raw Count"
        ),
        textInput(
          inputId = "sampleDistributionXlab",
          label = "X label",
          value = "Sample",
          placeholder = "Sample"
        ),
        textInput(
          inputId = "sampleDistributionYlab",
          label = "Y label",
          value = "log<sub>2</sub>(Count + 1)",
          placeholder = "log<sub>2</sub>(Count + 1)"
        )
      ),
      column(9,
             plotlyOutput("sampleDistributionBox") %>% withSpinner())
    ))
  } else {
    helpText("No data for ploting. Please import dataset and assign group information first.")
  }
})

# Filtering low count under different low number, barplot ----
output$lowCountFilterByCutoff <- renderPlotly({
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
    data <- tcc$count
    originalCount <- nrow(data)
    
    lowCount <-
      sapply(0:input$lowCountSlide, function(x) {
        nrow(filterLowCountGenes(tcc, low.count = x)$count)
      })
    
    lowCountdt <- data.frame(
      "Cutoff" = 0:input$lowCountSlide,
      "Filtered" = originalCount - lowCount,
      "Remain" = lowCount
    )
    
    plot_ly(
      lowCountdt,
      name = "Remain",
      x =  ~ Cutoff,
      y =  ~ Remain,
      text = ~ Remain,
      textposition = "outside",
      hoverinfo = "text+name",
      hovertext = ~ paste0(
        "Cut off: ",
        Cutoff,
        "<br>Filtered number: ",
        Filtered,
        "<br>Remain number: ",
        Remain,
        "(",
        round(Remain / nrow(data) * 100, 2),
        "%)"
      ),
      type = "bar"
    ) %>% add_trace(
      name = "Filtered",
      y =  ~ Filtered,
      text = ~ Filtered,
      type = "bar",
      textposition = "inside"
    )  %>% layout(
      title = "Filtering Threshold for Low Count Genes",
      barmode = "stack",
      xaxis = list(title = "Filtering Low Count Cut off"),
      yaxis = list(title = "Gene number")
    )
  } else {
    return()
  }
})

# Render Filtering Cutoff UI ----
output$lowCountFilterByCutoffUI <- renderUI({
  if (v$importActionValue) {
    tagList(fluidRow(
      column(
        3,
        helpText("Filter genes with a total read count smaller than thresholds."),
        sliderInput(
          inputId = "lowCountSlide",
          label = "Max threshold",
          min = 3,
          max = 50,
          value = 15,
          step = 1
        )
      ),
      column(9, plotlyOutput("lowCountFilterByCutoff") %>% withSpinner())
    ))
  } else {
    helpText("No data for ploting. Please import dataset and assign group information first.")
  }
})

# MDS Plot ----
output$mdsPlotObject <- renderPlotly({
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
    mds <-
      data.frame(cmdscale(dist(
        1 - cor(tcc$count, method = input$mdsMethod),
        method = input$mdsDistMethod
      )))
    mds$name <- rownames(mds)
    mdsG <- tcc$group
    mdsG$name <- rownames(mdsG)
    mdsJ <- left_join(mds, mdsG, by = "name")
    plot_ly(
      data = mdsJ,
      x = ~ X1,
      y = ~ X2,
      type = "scatter",
      mode = "text",
      text =  ~ name,
      color = ~ group
    ) %>% layout(title = "MDS Plot")
  } else {
    return()
  }
})

# Render MDS plot ----
output$mdsUI <- renderUI({
  if (v$importActionValue) {
    tagList(fluidRow(
      column(
        3,
        selectInput(
          inputId = "mdsDistMethod",
          label = "Distance Measure",
          choices = c(
            "Euclidean" = "euclidean",
            "Maximum" = "maximum",
            "Manhattan" = "manhattan",
            "Canberra" = "canberra",
            "Binary" = "binary",
            "Minkowski" = "minkowski"
          )
        ),
        selectInput(
          inputId = "mdsMethod",
          label = "Correlation Coefficient",
          choices = c(
            "Spearman" = "spearman",
            "Pearson" = "pearson",
            "Kendall" = "kendall"
          )
        )
      ),
      column(9, plotlyOutput("mdsPlotObject") %>% withSpinner())
    ))
  } else {
    helpText("No data for ploting. Please import dataset and assign group information first.")
  }
})

# Dend and heatmap ----
output$dendPlotObject <- renderPlotly({
  if (length(variables$tccObject) > 0) {
    tcc <- variables$tccObject
    data <- tcc$count[rowSums(tcc$count) > 0, ]
    data <- data.frame(1 - cor(data, method = input$dendCor))
    data.cl.count <- length(unique(tcc$group$group))
    heatmaply(
      data,
      k_col = data.cl.count,
      k_row = data.cl.count,
      hclust_method = input$dendCluster,
      labRow = rownames(data),
      labCol = colnames(data),
      colors = rev(GnBu(500))
    )
  } else {
    return()
  }
})

# Render dend and heatmap UI ----
output$dendUI <- renderUI({
  if (v$importActionValue) {
    tagList(fluidRow(
      column(
        3,
        selectInput(
          inputId = "dendCluster",
          label = "Distance Measure",
          choices = c(
            "Complete" = "complete",
            "Ward.D" = "ward.D",
            "Ward.D2" = "ward.D2",
            "Single" = "single",
            "UPGMA (Average)" = "average",
            "WPGMA (Mcquitty)" = "mcquitty",
            "WPGMC (Median)" = "median",
            "UPGMC (centroid)" = "centroid"
          )
        ),
        selectInput(
          inputId = "dendCor",
          label = "Correlation Coefficient",
          choices = c(
            "Spearman" = "spearman",
            "Pearson" = "pearson"
          )
        )
      ),
      column(9, plotlyOutput("dendPlotObject") %>% withSpinner())
    ))
  } else {
    helpText("No data for ploting. Please import dataset and assign group information first.")
  }
})