# server-expression-plot.R

runExp <- reactiveValues(runExpValue = FALSE)
# This function render a selectInput in expression paramters box ----

output$expressionParameters <- renderUI({
  tagList(
    textAreaInput(
      "expressionGeneList",
      "Paste Gene List",
      rows = 5,
      placeholder = "Input gene's name (first column in the dataset), one gene per line."
    ),
    do.call(actionBttn, c(
      list(
        inputId = "runExpression",
        label = "Generate Plot",
        icon = icon("play")
      ),
      actionBttnParams
    ))
  )
})

# Render plotly object of barplot -----
output$geneBarPlotExpression <- renderPlotly({
  if (length(variables$expressionData) > 0) {
    tcc <- variables$tccObject
    data <- variables$expressionData
    isolate({
      p <- list(0)
      xOrder <-
        data.frame(
          "name" = rownames(tcc$group),
          "group" = tcc$group$group
        ) %>% arrange(group)
      xform <- list(
        categoryorder = "array",
        categoryarray = xOrder$name,
        title = ""
      )

      for (i in 1:nrow(data)) {
        p[[i]] <- plot_ly(
          x = colnames(data),
          y = t(data[i, ]),
          color = xOrder[xOrder$name == colnames(data), ]$group,
          type = "bar",
          textposition = "auto",
          text = t(data[i, ]),
          insidetextfont = list(color = "white")
        ) %>%
          layout(
            annotations = list(
              x = 0.5,
              y = 1.1,
              text = row.names(data[i, ]),
              showarrow = F,
              xref = "paper",
              yref = "paper"
            ),
            showlegend = FALSE,
            xaxis = xform
          )
      }
      f <- list(0)
      j <- 1
      for (i in seq(1, nrow(data), 2)) {
        if (i + 1 <= nrow(data)) {
          f[[j]] <- subplot(p[[i]], p[[i + 1]])
        } else {
          f[[j]] <- subplot(p[[i]])
        }
        j <- j + 1
      }
      pp <- subplot(f, nrows = j - 1, margin = 0.05) %>%
        config(
          toImageButtonOptions = list(
            format = "svg",
            filename = "Expression_Plot"
          )
        )
      variables$expressionLevelBar <- pp
      pp
    })
  } else {
    return()
  }
})

# Render plotly object of boxplot -----
output$geneBoxPlotExpression <- renderPlotly({
  if (length(variables$expressionData) > 0) {
    tcc <- variables$tccObject
    data <- variables$expressionData
    isolate({
      p <- list(0)
      xOrder <-
        data.frame(
          "name" = rownames(tcc$group),
          "group" = tcc$group$group
        ) %>% arrange(group)

      for (i in 1:nrow(data)) {
        subdata <-
          data.frame("name" = rownames(t(data[i, ])), "value" = t(data[i, ])[, 1])
        subdata <- left_join(x = subdata, y = xOrder, by = "name")
        p[[i]] <- plot_ly(
          data = subdata,
          x = ~group,
          y = ~value,
          color = ~group,
          type = "box",
          boxpoints = "all",
          jitter = 0.3,
          pointpos = -1.8
        ) %>%
          layout(
            yaxis = list(rangemode = "tozero"),
            annotations = list(
              x = 0.5,
              y = 1,
              text = row.names(data[i, ]),
              showarrow = F,
              xref = "paper",
              yref = "paper"
            ),
            showlegend = FALSE
          )
      }
      f <- list(0)
      j <- 1
      for (i in seq(1, nrow(data), 2)) {
        if (i + 1 <= nrow(data)) {
          f[[j]] <- subplot(p[[i]], p[[i + 1]])
        } else {
          f[[j]] <- subplot(p[[i]])
        }
        j <- j + 1
      }
      pp <- subplot(f, nrows = j - 1, margin = 0.05) %>%
        config(
          toImageButtonOptions = list(
            format = "svg",
            filename = "Expression_Plot"
          )
        )
      variables$expressionLevelBox <- pp
      pp
    })
  } else {
    return()
  }
})

# Render plotly Bar and Box UI
output$geneBarPlotUI <- renderUI({
  if (runExp$runExpValue & !is.null(variables$expressionData)) {
    geneNum <- nrow(variables$expressionData)
    if (geneNum %/% 2) {
      height <- 300 * geneNum / 2
    } else {
      height <- 300 * (geneNum + 1) / 2
    }
    plotlyOutput("geneBarPlotExpression") %>% withSpinner()
  } else {
    helpText("Input gene list in [Expression Level Parameters] to generate barplot.")
  }
})

output$geneBoxPlotUI <- renderUI({
  if (runExp$runExpValue & !is.null(variables$expressionData)) {
    geneNum <- nrow(variables$expressionData)
    if (geneNum %/% 2) {
      height <- 300 * geneNum / 2
    } else {
      height <- 300 * (geneNum + 1) / 2
    }
    plotlyOutput("geneBoxPlotExpression") %>% withSpinner()
  } else {
    helpText("Input gene list in [Expression Level Parameters] to generate boxplot.")
  }
})

observeEvent(input$runExpression, {
  # Convert data for ploting -----
  runExp$runExpValue <- input$runExpression
  tcc <- variables$tccObject
  data <- data.frame(tcc$count)

  data <-
    data[row.names(data) %in% unlist(strsplit(x = input$expressionGeneList, split = "[\r\n]")), ]

  if (nrow(data) == 0) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Input list error, please make sure you have provided a correct list.",
      type = "error"
    )
    variables$expressionData <- NULL
    return()
  } else {
    variables$expressionData <- data
  }


  # R code of expression plot in barplot -----
  # output$expressionLevelCodeText <- renderText({
  #   code <- paste0(
  #     "# Dataset\n",
  #     "data <- ",
  #     paste0("c(", paste0("'", as.character(data.frame(round(data, 2))), "'", collapse = ",\n"), ")", collapse = ""),
  #     "\n\n# Gene name\n",
  #     "gene_name <- ",
  #     paste0("c(", paste0("'", colnames(data), "'", collapse = ","), ")", collapse = ""),
  #     "\n\n# Sample name\n",
  #     "sample_name <- ",
  #     paste0("c(", paste0("'", row.names(data), "'", collapse = ","), ")", collapse = ""),
  #     "\n\n# Group name\n",
  #     "group_name <- ",
  #     paste0("c(", paste0("'", data.cl, "'", collapse = ","), ")", collapse = ""),
  #     "\n\n# Convert vector to data.frame\nlibrary(tidyr)\nlibrary(plotly)\n",
  #     '\nvalue_row <- strsplit(x = gsub("c\\\\(", "", data), split = "(\\\\), )|(\\\\))")',
  #     "\nvalue_row <- data.frame(matrix(unlist(value_row)), row.names = gene_name)",
  #     '\ndata <- data.frame(value_row) %>% separate(colnames(value_row), sample_name, ",")',
  #     "\ndata <- data.frame(lapply(data,as.numeric), row.names = gene_name)",
  #     "\ndata <- t(data)",
  #     "\n\n# Barplot\n# Reorder x label (if necessary)\n",
  #     '\nxOrder <- data.frame("name" = colnames(data), "group" = group_name)',
  #     '\nxOrderVector <- unique(xOrder[order(xOrder$group),]$name)',
  #     '\nxform <- list(categoryorder = "array", categoryarray = xOrderVector, title = "")',
  #     '\np <- list(0)',
  #     '\n\nfor (i in 1:nrow(data)) {\n',
  #     '    p[[i]] <- plot_ly(x = colnames(data), y = t(data[i,]), color = factor(group_name), type = "bar")\n ',
  #     '    p[[i]] <- p[[i]] %>% layout(annotations = list(x = 0.5, y = 1.05, text = row.names(data)[i], showarrow = F, xref = "paper", yref = "paper" ),\n',
  #     '                                showlegend = FALSE, xaxis = xform)\n',
  #     '}\n',
  #     'f <- list(0)\n',
  #     'j <- 1\n',
  #     'for (i in seq(1, nrow(data), 2)) {\n',
  #     '    if(i + 1 <= nrow(data)){\n',
  #     '        f[[j]] <- subplot(p[[i]], p[[i+1]])\n',
  #     '    } else {\n',
  #     '        f[[j]] <- subplot(p[[i]])\n',
  #     '    }\n',
  #     '    j <- j + 1\n',
  #     '}\n',
  #     'subplot(f, nrows = j - 1, margin = 0.05)\n'
  #   )
  # })


  # R code of expression plot in boxplot
  # output$expressionLevelBoxCodeText <- renderText({
  #   code <- paste0(
  #     "# Dataset\n",
  #     "data <- ",
  #     paste0("c(", paste0("'", as.character(data.frame(round(data, 2))), "'", collapse = ",\n"), ")", collapse = ""),
  #     "\n\n# Gene name\n",
  #     "gene_name <- ",
  #     paste0("c(", paste0("'", colnames(data), "'", collapse = ","), ")", collapse = ""),
  #     "\n\n# Sample name\n",
  #     "sample_name <- ",
  #     paste0("c(", paste0("'", row.names(data), "'", collapse = ","), ")", collapse = ""),
  #     "\n\n# Group name\n",
  #     "group_name <- ",
  #     paste0("c(", paste0("'", data.cl, "'", collapse = ","), ")", collapse = ""),
  #     "\n\n# Convert vector to data.frame\nlibrary(tidyr)\nlibrary(plotly)\n",
  #     '\nvalue_row <- strsplit(x = gsub("c\\\\(", "", data), split = "(\\\\), )|(\\\\))")',
  #     "\nvalue_row <- data.frame(matrix(unlist(value_row)), row.names = gene_name)",
  #     '\ndata <- data.frame(value_row) %>% separate(colnames(value_row), sample_name, ",")',
  #     "\ndata <- data.frame(lapply(data,as.numeric), row.names = gene_name)",
  #     "\ndata <- t(data)",
  #     "\n\n# Barplot\n",
  #     '\np <- list(0)',
  #     '\n\nfor (i in 1:nrow(data)) {\n',
  #     '    p[[i]] <- plot_ly(x = colnames(data), y = t(data[i,]), color = factor(group_name), type = "bar")\n ',
  #     '    p[[i]] <- p[[i]] %>% layout(annotations = list(x = 0.5, y = 1.05, text = row.names(data)[i], showarrow = F, xref = "paper", yref = "paper" ),\n',
  #     '                                showlegend = FALSE, xaxis = xform)\n',
  #     '}\n',
  #     'f <- list(0)\n',
  #     'j <- 1\n',
  #     'for (i in seq(1, nrow(data), 2)) {\n',
  #     '    if(i + 1 <= nrow(data)){\n',
  #     '        f[[j]] <- subplot(p[[i]], p[[i+1]])\n',
  #     '    } else {\n',
  #     '        f[[j]] <- subplot(p[[i]])\n',
  #     '    }\n',
  #     '    j <- j + 1\n',
  #     '}\n',
  #     'subplot(f, nrows = j - 1, margin = 0.05)\n'
  #   )
  # })

  # Selected gene row count DataTable ----
  output$geneTable <- DT::renderDataTable({
    df <- data
    # Create 19 breaks and 20 rgb color values ranging from white to blue
    brks <-
      quantile(df %>% select_if(is.numeric),
        probs = seq(.05, .95, .05),
        na.rm = TRUE
      )

    DT::datatable(
      df,
      colnames = c("Gene Name" = 1),
      options = list(
        dom = "t",
        scrollX = TRUE
      )
    ) %>%
      formatStyle(names(df), backgroundColor = styleInterval(brks, head(Blues(40), n = length(brks) + 1)))
  })


  # Selected gene TCC result DataTable -----
  output$geneTableCal <- DT::renderDataTable({
    DT::datatable(
      resultTable()[resultTable()$gene_id %in% row.names(data), ],
      colnames = c(
        "Gene Name",
        "A Value",
        "M Value",
        "P Value",
        "Q Value (FDR)",
        "Rank",
        "estimated DEG"
      ),
      extensions = c("Buttons"),
      options = list(
        dom = "Bt",
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
        columnDefs = list(list(
          visible = FALSE, targets = -1
        ))
      )
    ) %>% formatRound(
      columns = c(
        "a.value",
        "m.value",
        "p.value",
        "q.value"
      ),
      digits = 3
    )
  })
  runExp$runExpValue <- input$runExpression
})
