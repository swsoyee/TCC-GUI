# server-ma-plot.R

runMA <- reactiveValues(runMAValues = FALSE)
# This function render a series UI of MA Plot parameters. ----

observeEvent(input$sider, {
  if (input$sider == "maplotTab") {
    output$MAPlotParameter <- renderUI({
      tagList(
        sliderInput(
          "pointSize",
          "Point Size",
          min = 1,
          max = 5,
          value = 3,
          step = 0.2
        ),
        sliderInput(
          "maFDR",
          "FDR Cut-off",
          min = 0.01,
          max = 1,
          value = input$fdr,
          step = 0.01
        ),
        spectrumInput(
          inputId = "fdrColor",
          label = tagList("DEGs Color", htmlOutput("maFDRpreview")),
          choices = list(
            list(
              "#B22222",
              "black",
              "white",
              "blanchedalmond",
              "steelblue",
              "forestgreen"
            ),
            as.list(brewer.pal(n = 9, name = "Reds")),
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
    })
  }
})

# Check the `Generate` button, if the botton has been clicked, generate MA Plot. ----

observeEvent(input$makeMAPlot, {
  output$maploty <- renderPlotly({
    validate(need(resultTable()$a.value != "", "No MA values for ploting."))

    req(input$makeMAPlot)
    isolate({
      # key for connecting MA Plot and Barplot
      key <- resultTable()$gene_id

      if (is.null(input$resultTableInPlot_rows_selected)) {
        annotation <- list()
      } else {
        markerSelect <-
          resultTable()[input$resultTableInPlot_rows_selected, ]

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

        p <- plot_ly(
          data = resultTable(),
          x = ~a.value,
          y = ~m.value,
          type = "scatter",
          mode = "markers",
          color = ~x,
          colors = c(input$fdrColor, "#000000"),
          marker = list(size = input$pointSize),
          hoverinfo = "text+name",
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
          key = ~key,
          source = "ma"
        ) %>%
          layout(
            xaxis = list(title = "A = (log<sub>2</sub>(G2)+log<sub>2</sub>(G1))/2"),
            yaxis = list(title = "M = log<sub>2</sub>(G2)-log<sub>2</sub>(G1)"),
            title = paste0(
              "MA Plot with q-value < ",
              input$maFDR,
              " (",
              input$maFDR * 100,
              "% FDR)"
            ),
            annotations = annotation,
            legend = list(
              orientation = "h",
              xanchor = "center",
              x = 0.5,
              y = 1.05
            )
          ) %>%
          config(toImageButtonOptions = list(
            format = "svg",
            filename = "MA_Plot"
          ))
        variables$MAPlotObject <- p
        p
      } else {
        p <- plot_ly(
          data = resultTable(),
          x = ~ as.numeric(a.value),
          y = ~ as.numeric(m.value),
          type = "scatter",
          mode = "markers",
          colors = c("#000000"),
          marker = list(size = input$pointSize),
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
          key = ~key,
          source = "ma"
        ) %>%
          layout(
            xaxis = list(title = "A = (log<sub>2</sub>(G2)+log<sub>2</sub>(G1))/2"),
            yaxis = list(title = "M = log<sub>2</sub>(G2)-log<sub>2</sub>(G1)"),
            title = "MA Plot",
            annotations = annotation,
            legend = list(
              orientation = "h",
              xanchor = "center",
              x = 0.5,
              y = 1.05
            )
          ) %>%
          config(toImageButtonOptions = list(
            format = "svg",
            filename = "MA_Plot"
          ))
        variables$MAPlotObject <- p
        p
      }
    })
  })
  runMA$runMAValues <- input$makeMAPlot
})

# Under FDR cutoff, preview the gene number ----
output$maFDRpreview <- renderText({
  count <- nrow(resultTable()[resultTable()$q.value <= input$maFDR, ])
  paste0(
    "<font color=\"",
    input$fdrColor,
    "\"><b>",
    count,
    " genes</b></font>"
  )
})

# Render MAPlotUI ----
output$MAPlotUI <- renderUI({
  if (runMA$runMAValues) {
    tagList(fluidRow(
      column(8, plotlyOutput("maploty") %>% withSpinner()),
      column(4, plotlyOutput("geneBarPlot") %>% withSpinner())
    ))
  } else {
    helpText("Please click [Generate MA Plot] first.")
  }
})

# This function render a button of R code of making MA plot. ----

observeEvent(input$makeMAPlot, {
  output$runMAPlot <- renderText({
    if (resultTable()$a.value == "") {
      "No MA values for plotting."
    }
    variables$runMAPlot
  })
})


# When hover on the point, show a expresion plot of specific gene. ----

output$geneBarPlot <- renderPlotly({
  # Read in hover data
  eventdata <- event_data("plotly_hover", source = "ma")
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
  data.cl <- variables$groupListConvert

  expression <- t(expression[data.cl != 0])
  data.cl <- data.cl[data.cl != 0]

  xOrder <-
    data.frame("name" = row.names(expression), "group" = data.cl)
  xOrderVector <- unique(xOrder[order(xOrder$group), ]$name)
  xform <- list(
    categoryorder = "array",
    categoryarray = xOrderVector,
    title = ""
  )

  plot_ly(
    x = ~ row.names(expression),
    y = ~ expression[, 1],
    color = as.factor(data.cl),
    text = expression[, 1],
    textposition = "outside",
    showlegend = FALSE,
    type = "bar",
    name = "Raw"
  ) %>%
    # add_trace(
    #   y = ~ expressionNor[, 1],
    #   text = round(expressionNor[, 1], 2),
    #   name = "Normalized",
    #   type = "scatter",
    #   mode = "markers",
    #   showlegend = FALSE,
    #   marker = list(
    #     size = 5,
    #     # line = list(color = "grey",
    #     #             width = 2),
    #     symbol = 4
    #   )
    # ) %>%
    layout(
      xaxis = xform,
      yaxis = list(title = "Raw Count"),
      title = colnames(expression)
    ) %>%
    config(toImageButtonOptions = list(
      format = "svg",
      filename = colnames(expression)
    ))
})


# This function render a table of Result table. ----

output$resultTableInPlot <- DT::renderDataTable({
  if (nrow(resultTable()) == 0) {
    DT::datatable(resultTable())
  } else {
    if (length(input$maFDR) > 0) {
      fdrCut <- input$maFDR
      fdrColor <- input$fdrColor
    } else {
      fdrCut <- 0
      fdrColor <- "#B22222"
    }

    DT::datatable(
      resultTable(),
      colnames = c(
        "Gene Name",
        "A Value",
        "M Value",
        "P Value",
        "Q Value (FDR)",
        "Rank",
        "estimated DEG"
      ),
      filter = "bottom",
      caption = tags$caption(
        tags$li(
          "Above buttons only deal with loaded part of the whole table (max to 99 rows)."
        ),
        tags$li(
          "Gene Name is colored according to FDR cut-off."
        ),
        tags$li(
          HTML("Please go to the <code>TCC Computation</code> tab and click on the <code>Download All Result (CSV)</code> button to download the table If you want a high-precision calculation result.")
        )
      ),
      extensions = c("Scroller", "Buttons"),
      option = list(
        dom = "Bfrtip",
        buttons =
          list(
            "copy",
            "print",
            list(
              extend = "collection",
              buttons = c("csv", "excel", "pdf"),
              text = "Download"
            )
          ),
        deferRender = TRUE,
        scrollY = 400,
        scroller = TRUE,
        searchHighlight = TRUE,
        orderClasses = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(
          visible = FALSE, targets = -1
        ))
      )
    ) %>%
      formatRound(
        columns = c(
          "a.value",
          "m.value",
          "p.value",
          "q.value"
        ),
        digits = 3
      ) %>%
      formatStyle("gene_id",
        "q.value",
        color = styleInterval(fdrCut, c(fdrColor, ""))
      )
  }
})


# Render a table of different gene count under specific FDR cutoff condition. ----

output$fdrCutoffTableInMAPage <- DT::renderDataTable({
  # Create Table
  df <- make_summary_for_tcc_result(resultTable())

  df <- df[, c("Cutoff", "Count", "Percentage")]
  colnames(df) <- c("Cut-off", "DEGs(#)", "DEGs(%)")

  # Render Table
  DT::datatable(
    df,
    caption = "Number (#) and Percentage (%) of DEGs satisfying different FDR cut-off.",
    option = list(
      pageLength = 10,
      columnDefs = list(list(
        className = "dt-right", targets = "_all"
      )),
      dom = "tp"
    ),
    rownames = FALSE
  )
})


# Render a plotly of different gene count under specific FDR cutoff ----

output$fdrCutoffPlotInMAPage <- renderPlotly({
  # Create table
  df <- make_summary_for_tcc_result(resultTable())

  # Render Plotly
  plot_ly(
    data = df,
    x = ~ as.numeric(Cutoff),
    y = ~Between_Count,
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
      y = ~Under_Count,
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
    ) %>%
    config(toImageButtonOptions = list(
      format = "svg",
      filename = "FDR_vs_DEGs"
    ))
})
