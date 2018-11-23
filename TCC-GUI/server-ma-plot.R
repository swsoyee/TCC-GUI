# server-ma-plot.R

# ====================================
# This function render a series UI of MA Plot parameters.
# Position: In [MA Plot tab], upper left.
# ====================================
# Input: sider (Click the tab in the left siderbar)
# Output: A series UI object (html tags)
# ====================================

observeEvent(input$sider, {
  if(input$sider == "maplotTab") {
  output$MAPlotParameter <- renderUI({
    tagList(
      sliderInput(
        "pointSize",
        "Point Size:",
        min = 1,
        max = 5,
        value = 3,
        step = 0.2
      ),
      sliderInput(
        "maFDR",
        "FDR:",
        min = 0,
        max = 1,
        value = input$fdr
      ),
      spectrumInput(
        inputId = "fdrColor",
        label = "DEGs color:",
        choices = list(
          list("#B22222", 'black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
          as.list(brewer.pal(n = 9, name = "Blues")),
          as.list(brewer.pal(n = 9, name = "Greens")),
          as.list(brewer.pal(n = 11, name = "Spectral")),
          as.list(brewer.pal(n = 8, name = "Dark2"))
        ),
        options = list(`toggle-palette-more-text` = "Show more")
      ),
      do.call(actionBttn, c(
        list(
          inputId = "makeMAPlot",
          label = "Generate MA Plot",
          icon = icon("play")
        ),
        actionBttnParams
      ))
    )
  })}
})

# ====================================
# This function check the `Generate` button, if the botton has been clicked,
# Generate MA Plot.
# Position: In [MA Plot tab], upper middle.
# ====================================
# Input: makeMAPlot (Botton)
# Output: Plotly object (Plot)
# ====================================

observeEvent(input$makeMAPlot, {
  withBars(output$maploty <- renderPlotly({
    validate(need(resultTable()$a.value != "", "No MA values for ploting."))
    
    req(input$makeMAPlot)
    isolate({
      # key for connecting MA Plot and Barplot
      key <- resultTable()$gene_id
      
      if (is.null(input$resultTableInPlot_rows_selected)) {
        annotation <- list()
      } else {
        markerSelect <-
          resultTable()[input$resultTableInPlot_rows_selected,]
        
        annotation <- list(
          x = markerSelect$a.value,
          y = markerSelect$m.value,
          text = markerSelect$gene_id,
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 20,
          ay = 40
        )
      }
      
      # If test method is `WAD`, it will not generate p.value,
      # So we can't pick up DEGs according to p.value, no color
      # information for marking the DEGs.
      if (input$testMethod != "wad") {
        x <- cut(resultTable()$q.value, breaks = c(0, input$maFDR, 1))
        levels(x) <-
          list(
            "DEG" = paste("(0,", input$maFDR, "]", sep = ""),
            "non-DEG" = paste("(", input$maFDR, ",1]", sep = "")
          )
        
        plot_ly(
          data = resultTable(),
          x = ~ a.value,
          y = ~ m.value,
          type = "scatter",
          mode = "markers",
          color = ~ x,
          colors = c(input$fdrColor, "#000000"),
          marker = list(size = 3),
          hoverinfo = "text",
          text = ~ paste(
            "</br>Gene:",
            resultTable()$gene_id,
            "</br>A value:",
            round(a.value, 4),
            "</br>M value:",
            round(m.value, 4),
            "</br>Rank:",
            rank
          ),
          key =  ~ key,
          source = "ma"
        ) %>%
          layout(
            xaxis = list(title = "A = (log2(G2)+log2(G1))/2"),
            yaxis = list(title = "M = log2(G2)-log2(G1)"),
            title = paste("MA Plot with FDR <", input$maFDR),
            annotations = annotation
          )
      } else {
        plot_ly(
          data = resultTable(),
          x = ~ as.numeric(a.value),
          y = ~ as.numeric(m.value),
          type = "scatter",
          mode = "markers",
          colors = c("#000000"),
          marker = list(size = 3),
          hoverinfo = "text",
          text = ~ paste(
            "</br>Gene:",
            resultTable()$gene_id,
            "</br>A value:",
            round(as.numeric(a.value), 4),
            "</br>M value:",
            round(as.numeric(m.value), 4),
            "</br>Rank:",
            rank
          ),
          key =  ~ key,
          source = "ma"
        ) %>%
          layout(
            xaxis = list(title = "A = (log2(G2)+log2(G1))/2"),
            yaxis = list(title = "M = log2(G2)-log2(G1)"),
            title = "MA Plot",
            annotations = annotation
          )
      }
    })
  }))
})

# ====================================
# This function render a button of R code of making MA plot.
# Position: In [MA Plot tab], down right
# ====================================
# Input: makeMAPlot (Botton)
# Output: R Code (Text)
# ====================================

observeEvent(input$makeMAPlot, {
  output$runMAPlot <- renderText({
    if(resultTable()$a.value == "") {
      "No MA values for plotting."
    }
    variables$runMAPlot
  })
})

# ====================================
# When hover on the point, show a expresion plot of specific gene.
# Position: In [MA Plot tab], up right
# ====================================
# Input: None (Hover on point)
# Output: Plotly object (Plot)
# ====================================

withBars(output$geneBarPlot <- renderPlotly({
  # Read in hover data
  eventdata <- event_data("plotly_hover", source = "ma")
  validate(need(
    !is.null(eventdata),
    "Hover over the point to show expression plot"
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
      title = paste(colnames(expression), "Expression Plot"),
      legend = list(orientation = 'h')
    )
}))

# ====================================
# This function render a table of Result table.
# Position: In [MA plot tab] and In [Volcano plot tab], under middle.
# ====================================
# Input: None
# Output: DataTable object (Table)
# ====================================

output$resultTableInVolcanalPlot <-
  output$resultTableInPlot <- DT::renderDataTable({
    if (nrow(resultTable()) == 0) {
      DT::datatable(resultTable())
    } else {
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
      )
    }
  })

# ====================================
# This function render a table of different gene count under specific FDR cutoff
# condition.
# Position: In [MA plot tab] and In [Volcano plot tab], under left.
# ====================================
# Input: None
# Output: DataTable object (Table)
# ====================================

output$fdrCutoffTableInVolcano <-
  output$fdrCutoffTableInMAPage <- DT::renderDataTable({
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
# Position: In [MA plot tab], under left.
# ====================================
# Input: None
# Output: Plotly object (Plot)
# ====================================
withBars(output$fdrCutoffPlotInMAPage <- renderPlotly({
  # Create table
  df <- make_summary_for_tcc_result(resultTable())
  
  # Render Plotly
  plot_ly(
    data = df,
    x = ~ as.numeric(Cutoff),
    y = ~ Between_Count,
    type = "bar",
    hoverinfo = "text",
    text = ~ paste("</br>FDR cutoff: ", Cutoff,
                   "</br>DEGs between cutoff: ", Between_Count,
                   "</br>Total DEGs under cufoff: ", Under_Count)
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
      xaxis = list(title = "FDR Cutoff (%)", 
                   tickvals = c(2, 4, 6), 
                   ticktext = c(1, 10, 20)),
      yaxis = list(title = "DEGs count between cutoff"),
      yaxis2 = list(title = "Total DEGs under cufoff", overlaying = "y", side = "right"),
      showlegend = FALSE,
      margin = list(r = 50)
    )
}))