# server-volcano-plot.R


# This function render a series UI of Volcano Plot parameters ----

observeEvent(input$sider, {
  if(input$sider == "volcanoplotTab"){
  # If test method is `WAD`, it will not generate p.value,
  # So we can't plot Volcano Plot. If is not `WAD`,
  # generate volcano plot parameters.
  if (input$testMethod != "wad") {
    output$valcanoParameter <- renderUI({
      tagList(
        sliderInput(
          "CutFC",
          "Fold Change Cut-off",
          min = -10,
          max = 10,
          value = c(-1, 1),
          step = 0.5
        ),
        textInput("Cutpvalue", "P-value Cut-off", value = 0.05),
        sliderInput(
          "pointSize",
          "Point Size",
          min = 1,
          max = 5,
          value = 3,
          step = 0.2
        ),
        textInput("xlabs", "X-axis (Fold Change) Label", value = "log2(Fold Change)"),
        textInput("ylabs", "Y-axis (P-value) Label", value = "-log10(P-value)"),
        textInput("graphicTitle", "Graphic Title", value = "Volcano Plot"),
        
        column(6,
        spectrumInput(
          inputId = "downColor",
          label = "Down-regulate",
          choices = list(
            list("green", 'black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
            as.list(brewer.pal(n = 9, name = "Blues")),
            as.list(brewer.pal(n = 9, name = "Greens")),
            as.list(brewer.pal(n = 11, name = "Spectral")),
            as.list(brewer.pal(n = 8, name = "Dark2"))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        )),
        column(6,
        spectrumInput(
          inputId = "upColor",
          label = "Up-regulate",
          choices = list(
            list("red", 'black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
            as.list(brewer.pal(n = 9, name = "Blues")),
            as.list(brewer.pal(n = 9, name = "Greens")),
            as.list(brewer.pal(n = 11, name = "Spectral")),
            as.list(brewer.pal(n = 8, name = "Dark2"))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        )),
        do.call(actionBttn, c(
          list(
            inputId = "makeVolcanoPlot",
            label = "Generate Volcano Plot",
            icon = icon("play")
          ),
          actionBttnParams
        ))
      )
    })
  } else {
    output$valcanoParameter <- renderUI({
      tagList(
        tags$p(
          "No p-value and q-value for plotting when \"WAD\" test method was selected."
        )
      )
    })
  }}
})

# Check the `Generate` button, if the botton has been clicked, generate volcano plot ----

observeEvent(input$makeVolcanoPlot, {
  withBars(output$volcanoPloty <- renderPlotly({
    validate(need(resultTable()$p.value != "", "No p-values for ploting."))
    if(length(variables$groupList) > 2) {
      sendSweetAlert(
        session = session,
        title = "Plot error!",
        text = "Volcano Plot is unavailable for multiple comparison now.",
        type = "info"
      )
    }
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
      
      p <- plot_ly(
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
      variables$VolcanoPlotObject <- p
      p 
    })
  }))
})


# This function render a button of R code of making vocalno plot ----

observeEvent(input$makeVolcanoPlot, {
  output$runVolcanoPlot <- renderText({
    variables$runVolcanoPlot
  })
})


# This function render a plotly of specific gene expression value in barplot.----

withBars(output$geneBarPlotInVolcano <- renderPlotly({
  # Read in hover data
  eventdata <- event_data("plotly_hover", source = "volcano")
  validate(need(
    !is.null(eventdata),
    "Hover over the point to show gene's expression level of interest."
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
  data.cl<- variables$groupListConvert
  
  expression <- t(expression[data.cl != 0])
  data.cl <- data.cl[data.cl != 0]
  
  xOrder <- data.frame("name" = row.names(expression), "group" = data.cl)
  xOrderVector <- unique(xOrder[order(xOrder$group), ]$name)
  xform <- list(categoryorder = "array",
                categoryarray = xOrderVector,
                title = "")
  
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
      xaxis = xform,
      yaxis = list(title = "Count"),
      title = colnames(expression),
      legend = list(orientation = 'h')
    )
}))


# Render a plotly of different gene count under specific FDR cutoff condition.----

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
    text = ~ paste("</br>FDR Cut-off: ", Cutoff,
                   "</br>DEGs between Cut-off: ", Between_Count,
                   "</br>Total DEGs under Cuf-off: ", Under_Count)
  ) %>%
    add_trace(
      y = ~ Under_Count,
      yaxis = "y2",
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~ paste(
        "</br>FDR Cut-off: ",
        Cutoff,
        "</br>Cumulative curve: ",
        Percentage
      )
    ) %>%
    layout(
      xaxis = list(title = "FDR Cut-off", 
                   tickvals = c(2, 4, 6), 
                   ticktext = c("0.01", "0.10", "0.20")),
      yaxis = list(title = "DEGs (#) between cut-offs", 
                   rangemode = "nonnegative",
                   titlefont = list(color = "#1F77B4")),
      yaxis2 = list(title = "Cumulative number of DEGs", 
                    titlefont = list(color = "#FF7F0E"),
                    rangemode = "nonnegative",
                    overlaying = "y", 
                    side = "right"),
      showlegend = FALSE,
      margin = list(r = 50)
    )
}))