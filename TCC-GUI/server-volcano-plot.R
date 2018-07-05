# server-volcano-plot.R

# ====================================
# This function render a series UI of Volcano Plot parameters.
#
# Position: In Volcano Plot tab, upper left.
# ====================================

observeEvent(input$TCC, {
  # If test method is `WAD`, it will not generate p.value,
  # So we can't plot Volcano Plot. If is not `WAD`,
  # generate volcano plot parameters.
  if (input$testMethod != "wad") {
    output$valcanoParameter <- renderUI({
      tagList(
        sliderInput(
          "CutFC",
          "Fold Change cut-off：",
          min = -10,
          max = 10,
          value = c(-1, 1),
          step = 0.5
        ),
        textInput("Cutpvalue", "p-value cut-off：", value = 0.05),
        sliderInput(
          "pointSize",
          "Point Size:",
          min = 1,
          max = 5,
          value = 3,
          step = 0.2
        ),
        textInput("xlabs", "X-axis (Fold Change) label:", value = "log2(Fold Change)"),
        textInput("ylabs", "Y-axis (P-value) label:", value = "-log10(P-value)"),
        textInput("graphicTitle", "Graphic Title", value = "Volcano Plot"),
        colourInput("downColor", "Down-regulate：", "green"),
        colourInput("upColor", "Up-regulate：", "red"),
        fluidRow(column(
          6,
          actionButton("makeVolcanoPlot", "Generate Volcano Plot")
        ),
        column(6, uiOutput(
          "runVolcanoPlot"
        )))
      )
    })
  } else {
    output$valcanoParameter <- renderUI({
      tagList(
        tags$p(
          "Because you have chosen \"WAD\" test method, there is no p-value and q-value result for plotting."
        )
      )
    })
  }
})

# ====================================
# This function check the `Generate` button, if the botton is clicked,
# Generate volcano plot.
# Position: In Volcano Plot tab, upper middle.
# ====================================

observeEvent(input$makeVolcanoPlot, {
  withBars(output$volcanoPloty <- renderPlotly({
    validate(need(resultTable()$p.value != "", "No p-values for ploting."))
    
    req(input$makeVolcanoPlot)
    isolate({
      dt <- resultTable()
      
      downCut <- input$CutFC[1]
      upCut <- input$CutFC[2]
      pCut <- log2(as.numeric(input$Cutpvalue))
      
      dt$color <- ""
      dt[dt$m.value <= downCut, ]$color <- "Down"
      dt[dt$m.value >= upCut, ]$color <- "Up"
      dt[dt$p.value > as.numeric(input$Cutpvalue), ]$color <- "None"
      dt[dt$m.value <= upCut &
           dt$m.value >= downCut, ]$color <- "None"
      
      x <- factor(dt$color)
      levels(x) <- list("Down" = 0,
                        "None" = 1,
                        "Up" = 2)
      
      # Add annotation
      key <- resultTable()$gene_id
      
      if (is.null(input$resultTableInVolcanalPlot_rows_selected)) {
        annotation <- list()
      } else {
        markerSelect <- dt[input$resultTableInVolcanalPlot_rows_selected,]
        
        annotation <- list(
          x = markerSelect$m.value,
          y = -log10(markerSelect$p.value),
          text = markerSelect$gene_id,
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 20,
          ay = 40
        )
      }
      
      plot_ly(
        data = dt,
        x = ~ m.value,
        y = ~ -log10(p.value),
        type = "scatter",
        mode = "markers",
        color = ~ x,
        colors = c(input$downColor, "black", input$upColor),
        marker = list(size = 3),
        hoverinfo = "text",
        text = ~ paste(
          "</br>Gene:",
          resultTable()$gene_id,
          "</br>A value:",
          round(a.value, 4),
          "</br>M value:",
          round(m.value, 4),
          "</br>p-value:",
          round(p.value, 4),
          "</br>q-value:",
          round(q.value, 4),
          "</br>Rank:",
          rank
        ),
        key =  ~ key,
        source = "volcano"
      ) %>%
        layout(
          xaxis = list(title = input$xlabs),
          yaxis = list(title = input$ylabs),
          title = input$graphicTitle,
          annotations = annotation,
          shapes = list(
            list(
              type = 'line',
              y0 =  ~ min(-log10(p.value)),
              y1 =  ~ max(-log10(p.value)),
              x0 = upCut,
              x1 = upCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line',
              y0 =  ~ min(-log10(p.value)),
              y1 =  ~ max(-log10(p.value)),
              x0 = downCut,
              x1 = downCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line',
              y0 = -log10(as.numeric(input$Cutpvalue)),
              y1 = -log10(as.numeric(input$Cutpvalue)),
              x0 =  ~ min(m.value),
              x1 =  ~ max(m.value),
              line = list(dash = 'dot', width = 2)
            )
          )
        )
    })
  }))
  
  # ====================================
  # This function render a button of R code of making vocalno plot.
  # Position: In Volcano Plot, upper left, parameter panel.
  # ====================================
  
  output$runVolcanoPlot <- renderUI({
    actionButton("showVolcanoCode", "Show R code")
  })
})

# ====================================
# This function popup a window of R code of making vocalno plot.
# Position: In Volcano Plot, middle.
# ====================================

observeEvent(input$showVolcanoCode, {
  shinyalert(
    title = "Volcano Plot code",
    text = variables$runVolcanoPlot,
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

# ====================================
# This function render a plotly of specific gene expression value in barplot.
# Position: In Volcano Plot, upper right.
# ====================================
withBars(output$geneBarPlotInVolcano <- renderPlotly({
  # Read in hover data
  eventdata <- event_data("plotly_hover", source = "volcano")
  validate(need(
    !is.null(eventdata),
    "Hover over the point to show original expression plot"
  ))
  # Get point number
  gene_id <- eventdata$key
  # Get expression level (Original)
  expression <-
    variables$CountData[row.names(variables$CountData) == gene_id, ]
  # Get expression level (Normalized)
  expressionNor <-
    t(t(variables$norData[row.names(variables$norData) == gene_id, ]))
  
  data <- variables$CountData
  data.cl <- rep(0, ncol(data))
  
  for (i in 1:length(variables$groupList)) {
    data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = data))] = i
  }
  
  expression <- t(expression[data.cl != 0])
  data.cl <- data.cl[data.cl != 0]
  
  plot_ly(
    x = ~ row.names(expression),
    y = ~ expression[, 1],
    color = as.factor(data.cl),
    text = expression[, 1],
    textposition = 'auto',
    type = "bar",
    name = "Original"
  ) %>%
    add_trace(
      y = ~ expressionNor[, 1],
      text = round(expressionNor[, 1], 2),
      textposition = 'auto',
      name = "Normalized",
      type = "scatter",
      mode = "lines+markers",
      marker = list(
        size = 10,
        line = list(color = 'rgba(0, 0, 0, 0)',
                    width = 2)
      )
    ) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Raw Count"),
      title = paste(colnames(expression), "Expression Plot"),
      legend = list(orientation = 'h')
    )
}))

# ====================================
# This function render a table of different gene count under specific FDR cutoff
# condition.
# Position: In Volcano Plot, under right.
# ====================================

output$fdrCutoffTableInVolcano <- DT::renderDataTable({
  # Create Table
  df <- make_summary_for_tcc_result(resultTable())
  
  # Render Table
  DT::datatable(
    df[, c("Cutoff", "Count", "Percentage")],
    option = list(
      pageLength = 10,
      columnDefs = list(list(
        className = 'dt-right', targets = "_all"
      )),
      dom = "tp"
    ),
    rownames = FALSE
  )
})

# ====================================
# This function render a plotly of different gene count under specific FDR cutoff
# condition.
# Position: In Volcano Plot tab, under right.
# ====================================
withBars(output$fdrCutoffPlotInVolcano <- renderPlotly({
  # Create table
  df <- make_summary_for_tcc_result(resultTable())
  
  # Render Plotly
  plot_ly(
    data = df,
    x = ~ as.numeric(Cutoff),
    y = ~ Between_Count,
    type = "bar",
    hoverinfo = "text",
    text = ~ paste("</br>FDR Cutoff: ", Cutoff,
                   "</br>DEGs Count: ", Between_Count)
  ) %>%
    add_trace(
      y = ~ Under_Count,
      yaxis = "y2",
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~ paste(
        "</br>FDR Cutoff: ",
        Cutoff,
        "</br>Cumulative curve: ",
        Percentage
      )
    ) %>%
    layout(
      xaxis = list(title = "FDR Cutoff"),
      yaxis = list(title = "DEGs Count"),
      yaxis2 = list(overlaying = "y", side = "right"),
      showlegend = FALSE
    )
}))