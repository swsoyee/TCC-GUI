# server-volcano-plot.R

# This function render a series UI of Volcano Plot parameters ----

observeEvent(input$sider, {
  if (input$sider == "volcanoplotTab") {
    # If test method is `WAD`, it will not generate p.value,
    # So we can't plot Volcano Plot. If is not `WAD`,
    # generate volcano plot parameters.
    if (input$testMethod != "wad") {
      output$valcanoParameter <- renderUI({
        tagList(
          sliderInput(
            "CutFC",
            "Fold Change (X-axis) Cut-off",
            min = ceiling(min(resultTable()$m.value)),
            max = floor(max(resultTable()$m.value)),
            value = c(-1, 1),
            step = 0.5
          ),
          textInput("xlabs", "X-axis Label", value = "log<sub>2</sub>(Fold Change)"),
          # radioGroupButtons(inputId = "volcanoYcolumn",
          #                   label = "Y-axis Value",
          #                   choices = c("P-value" = "p.value",
          #                               "Q-value" = "q.value"),
          #                   status = "primary",
          #                   justified = TRUE),
          sliderInput(
            inputId = "Cutpvalue",
            label = "P-value Cut-off",
            min = 0.01,
            value = 0.05,
            max = 1,
            step = 0.01
          ),
          textInput("ylabs", "Y-axis Label", value = "-log<sub>10</sub>(P-value)"),
          sliderInput(
            "volcanoPointSize",
            "Point Size",
            min = 1,
            max = 5,
            value = 3,
            step = 0.2
          ),
          textInput("graphicTitle", "Graphic Title", value = "Volcano Plot"),
          spectrumInput(
            inputId = "downColor",
            label = tagList("Down-regulate in G2", htmlOutput("downPreview")),
            choices = list(
              list(
                "green",
                'black',
                'white',
                'blanchedalmond',
                'steelblue',
                'forestgreen'
              ),
              as.list(brewer.pal(n = 9, name = "Blues")),
              as.list(brewer.pal(n = 9, name = "Greens")),
              as.list(brewer.pal(n = 11, name = "Spectral"))
            ),
            options = list(`toggle-palette-more-text` = "Show more")
            
          ),
          spectrumInput(
            inputId = "upColor",
            label = tagList("Up-regulate in G2", htmlOutput("upPreview")),
            choices = list(
              list(
                "red",
                'black',
                'white',
                'blanchedalmond',
                'steelblue',
                'forestgreen'
              ),
              as.list(brewer.pal(n = 9, name = "Oranges")),
              as.list(brewer.pal(n = 9, name = "Reds")),
              as.list(brewer.pal(n = 11, name = "Spectral"))
            ),
            options = list(`toggle-palette-more-text` = "Show more")
          ),
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
    }
  }
})

observeEvent({
  input$CutFC
  input$Cutpvalue
}, {
  dt <- resultTable()
  downCut <- input$CutFC[1]
  upCut <- input$CutFC[2]
  pvalueCut <- input$Cutpvalue
  
  downCount <-
    nrow(dt[dt$m.value <= downCut & dt[["p.value"]] <= pvalueCut,])
  upCount <-
    nrow(dt[dt$m.value >= upCut & dt[["p.value"]] <= pvalueCut,])
  
  output$downPreview <- renderText({
    paste0("<font color=\"",
           input$downColor,
           "\"><b>",
           downCount,
           " genes</b></font>")
  })
  output$upPreview <- renderText({
    paste0("<font color=\"",
           input$upColor,
           "\"><b>",
           upCount,
           " genes</b></font>")
  })
})

# Select y axis column ----
# observeEvent(input$volcanoYcolumn, {
#   if (input$volcanoYcolumn == "p.value") {
#     label <- "P-value Cut-off"
#     value <- "-log<sub>10</sub>(P-value)"
#     updateSliderInput(session = session, inputId = "Cutpvalue", label = label)
#     updateTextInput(session = session, inputId = "ylabs", value = value)
#   } else {
#     label <- "Q-value (Adjusted P-value) Cut-off"
#     value <- "-log<sub>10</sub>(Q-value)"
#     updateSliderInput(session = session, inputId = "Cutpvalue", label = label)
#     updateTextInput(session = session, inputId = "ylabs", value = value)
#   }
# })

# Check the `Generate` button, if the botton has been clicked, generate volcano plot ----

observeEvent(input$makeVolcanoPlot, {
  # yaxis <- isolate(input$volcanoYcolumn)
  yaxis <- "p.value"
  withBars(output$volcanoPloty <- renderPlotly({
    validate(need(resultTable()[[yaxis]] != "", "No p-values for ploting."))
    if (length(variables$groupList) > 2) {
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "Volcano Plot is unavailable for multiple comparison now.",
        type = "info"
      )
    }
    
    req(input$makeVolcanoPlot)
    isolate({
      dt <- resultTable()
      
      downCut <- input$CutFC[1]
      upCut <- input$CutFC[2]
      
      dt$color <- "None"
      tryCatch({
        dt[dt$m.value <= downCut,]$color <- "Down"
        dt[dt$m.value >= upCut,]$color <- "Up"
        dt[dt[[yaxis]] > input$Cutpvalue,]$color <-
          "None"
      }, error = function(e) {
        sendSweetAlert(title = "ERROR", text = "No data was satisfied to your cut-off!")
      })
      
      x <- factor(dt$color)
      levels(x) <- list("Down" = 0,
                        "None" = 1,
                        "Up" = 2)
      
      # Add annotation
      key <- resultTable()$gene_id
      
      if (is.null(input$resultTableInVolcanalPlot_rows_selected)) {
        annotation <- list()
      } else {
        markerSelect <- dt[input$resultTableInVolcanalPlot_rows_selected, ]
        
        annotation <- list(
          x = markerSelect$m.value,
          y = -log10(markerSelect[[yaxis]]),
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
        y = ~ -log10(dt[[yaxis]]),
        type = "scatter",
        mode = "markers",
        color = ~ x,
        colors = c(input$downColor, "black", input$upColor),
        marker = list(size = input$volcanoPointSize),
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
          legend = list(
            orientation = 'h',
            xanchor = "center",
            x = 0.5,
            y = 1.05
          ),
          annotations = annotation,
          shapes = list(
            list(
              type = 'line',
              y0 =  ~ min(-log10(dt[[yaxis]])),
              y1 =  ~ max(-log10(dt[[yaxis]])),
              x0 = upCut,
              x1 = upCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line',
              y0 =  ~ min(-log10(dt[[yaxis]])),
              y1 =  ~ max(-log10(dt[[yaxis]])),
              x0 = downCut,
              x1 = downCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line',
              y0 = -log10(input$Cutpvalue),
              y1 = -log10(input$Cutpvalue),
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

# Render table under volcano plot



output$resultTableInVolcanalPlot <- DT::renderDataTable({
  if (nrow(resultTable()) == 0) {
    DT::datatable(resultTable())
  } else {
    if (length(input$Cutpvalue) > 0) {
      fdrCut <- input$Cutpvalue
    } else {
      fdrCut <- 0
    }
    
    DT::datatable(
      resultTable(),
      option = list(
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE,
        scrollX = TRUE
      )
    ) %>% formatRound(
      columns = c("a.value",
                  "m.value",
                  "p.value",
                  "q.value"),
      digits = 3
    ) %>% formatStyle("estimatedDEG",
                      target = 'row',
                      backgroundColor = styleEqual(1, "lightblue")) %>% formatStyle("gene_id", "m.value",
                                                                                    color = styleInterval(input$CutFC,
                                                                                                          c(
                                                                                                            input$downColor, "black", input$upColor
                                                                                                          ))) %>% formatStyle("gene_id",
                                                                                                                              "p.value",
                                                                                                                              fontWeight = styleInterval(fdrCut, c("bold", "normal")))
  }
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
    variables$CountData[row.names(variables$CountData) == gene_id,]
  # Get expression level (Normalized)
  expressionNor <-
    t(t(variables$norData[row.names(variables$norData) == gene_id,]))
  
  data <- variables$CountData
  data.cl <- variables$groupListConvert
  
  expression <- t(expression[data.cl != 0])
  data.cl <- data.cl[data.cl != 0]
  
  xOrder <-
    data.frame("name" = row.names(expression), "group" = data.cl)
  xOrderVector <- unique(xOrder[order(xOrder$group),]$name)
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
    text = ~ paste(
      "</br>FDR Cut-off: ",
      Cutoff,
      "</br>DEGs between Cut-off: ",
      Between_Count,
      "</br>Total DEGs under Cuf-off: ",
      Under_Count
    )
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
      xaxis = list(
        title = "FDR Cut-off",
        tickvals = c(1, 3, 5),
        ticktext = c("0", "0.10", "0.20")
      ),
      yaxis = list(
        title = "DEGs (#) between cut-offs",
        rangemode = "nonnegative",
        titlefont = list(color = "#1F77B4")
      ),
      yaxis2 = list(
        title = "Cumulative number of DEGs",
        titlefont = list(color = "#FF7F0E"),
        rangemode = "nonnegative",
        overlaying = "y",
        side = "right"
      ),
      showlegend = FALSE,
      margin = list(r = 50)
    )
}))