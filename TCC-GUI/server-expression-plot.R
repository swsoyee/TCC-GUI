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

observeEvent(input$runExpression, {
    # Convert data for ploting -----
    data <- variables$CountData
    data.cl <- variables$groupListConvert
    
    data <-
      data[row.names(data) %in% unlist(strsplit(x = input$expressionGeneList, split = '[\r\n]')),]
    data <- data[, data.cl != 0]
    data.cl <- data.cl[data.cl != 0]
    
    
    # Render plotly object of barplot -----
    output$geneBarPlotExpression <- renderPlotly({
      isolate({
      p <- list(0)
      
      if (input$expressionGeneList == "") {
        sendSweetAlert(
          session = session,
          title = "Gene list error!",
          text = "No such data in your dataset! Please check your input!",
          type = "error"
        )
        return()
      }
      
      xOrder <-
        data.frame("name" = colnames(data), "group" = data.cl)
      xOrderVector <- unique(xOrder[order(xOrder$group),]$name)
      xform <- list(
        categoryorder = "array",
        categoryarray = xOrderVector,
        title = ""
      )
      
      for (i in 1:nrow(data)) {
        p[[i]] <- plot_ly(
          x = colnames(data),
          y = t(data[i,]),
          color = as.factor(data.cl),
          type = "bar"
        ) %>%
          layout(
            annotations = list(
              x = 0.5,
              y = 1.05,
              text = row.names(data[i,]),
              showarrow = F,
              xref = 'paper',
              yref = 'paper'
            ),
            showlegend = FALSE,
            xaxis = xform
          )
      }
      f <- list(0)
      j <- 1
      for (i in seq(1, nrow(data), 2)) {
        if(i + 1 <= nrow(data)){
          f[[j]] <- subplot(p[[i]], p[[i+1]])
        } else {
          f[[j]] <- subplot(p[[i]])
        }
        j <- j + 1
      }
      pp <- subplot(f, nrows = j - 1, margin = 0.05)
      variables$expressionLevelBar <- pp
      pp
      })
      
    })

    
    # Render plotly object of boxplot -----
    
    output$geneBoxPlotExpression <- renderPlotly({
      isolate({
      p <- list(0)
      
      if (input$expressionGeneList == "") {
        sendSweetAlert(
          session = session,
          title = "Gene list error!",
          text = "No such data in your dataset! Please check your input!",
          type = "error"
        )
        return()
      }
      
      for (i in 1:nrow(data)) {
        p[[i]] <- plot_ly(
          x = as.factor(data.cl),
          y = t(data[i,]),
          color = as.factor(data.cl),
          type = "box",
          boxpoints = "all",
          jitter = 0.3,
          pointpos = -1.8
        ) %>%
          layout(
            annotations = list(
              x = 0.5,
              y = 1.05,
              text = row.names(data[i,]),
              showarrow = F,
              xref = 'paper',
              yref = 'paper'
            ),
            showlegend = FALSE
          )
      }
      f <- list(0)
      j <- 1
      for (i in seq(1, nrow(data), 2)) {
        if(i + 1 <= nrow(data)){
          f[[j]] <- subplot(p[[i]], p[[i+1]])
        } else {
          f[[j]] <- subplot(p[[i]])
        }
        j <- j + 1
      }
      pp <- subplot(f, nrows = j - 1, margin = 0.05)
      variables$expressionLevelBox <- pp
      pp
      })
    })
    
    
    
    # R code of expression plot in barplot -----
    output$expressionLevelCodeText <- renderText({
      code <- paste0(
        "# Dataset\n",
        "data <- ",
        paste0("c(", paste0("'", as.character(data.frame(round(data, 2))), "'", collapse = ",\n"), ")", collapse = ""),
        "\n\n# Gene name\n",
        "gene_name <- ",
        paste0("c(", paste0("'", colnames(data), "'", collapse = ","), ")", collapse = ""),
        "\n\n# Sample name\n",
        "sample_name <- ",
        paste0("c(", paste0("'", row.names(data), "'", collapse = ","), ")", collapse = ""),
        "\n\n# Group name\n",
        "group_name <- ",
        paste0("c(", paste0("'", data.cl, "'", collapse = ","), ")", collapse = ""),
        "\n\n# Convert vector to data.frame\nlibrary(tidyr)\nlibrary(plotly)\n",
        '\nvalue_row <- strsplit(x = gsub("c\\\\(", "", data), split = "(\\\\), )|(\\\\))")',
        "\nvalue_row <- data.frame(matrix(unlist(value_row)), row.names = gene_name)",
        '\ndata <- data.frame(value_row) %>% separate(colnames(value_row), sample_name, ",")',
        "\ndata <- data.frame(lapply(data,as.numeric), row.names = gene_name)",
        "\ndata <- t(data)",
        "\n\n# Barplot\n# Reorder x label (if necessary)\n",
        '\nxOrder <- data.frame("name" = colnames(data), "group" = group_name)',
        '\nxOrderVector <- unique(xOrder[order(xOrder$group),]$name)',
        '\nxform <- list(categoryorder = "array", categoryarray = xOrderVector, title = "")',
        '\np <- list(0)',
        '\n\nfor (i in 1:nrow(data)) {\n',
        '    p[[i]] <- plot_ly(x = colnames(data), y = t(data[i,]), color = factor(group_name), type = "bar")\n ',
        '    p[[i]] <- p[[i]] %>% layout(annotations = list(x = 0.5, y = 1.05, text = row.names(data)[i], showarrow = F, xref = "paper", yref = "paper" ),\n',
        '                                showlegend = FALSE, xaxis = xform)\n',
        '}\n',
        'f <- list(0)\n',
        'j <- 1\n',
        'for (i in seq(1, nrow(data), 2)) {\n',
        '    if(i + 1 <= nrow(data)){\n',
        '        f[[j]] <- subplot(p[[i]], p[[i+1]])\n',
        '    } else {\n',
        '        f[[j]] <- subplot(p[[i]])\n',
        '    }\n',
        '    j <- j + 1\n',
        '}\n',
        'subplot(f, nrows = j - 1, margin = 0.05)\n'
      )
    })
    
    
    # R code of expression plot in boxplot
    output$expressionLevelBoxCodeText <- renderText({
      code <- paste0(
        "# Dataset\n",
        "data <- ",
        paste0("c(", paste0("'", as.character(data.frame(round(data, 2))), "'", collapse = ",\n"), ")", collapse = ""),
        "\n\n# Gene name\n",
        "gene_name <- ",
        paste0("c(", paste0("'", colnames(data), "'", collapse = ","), ")", collapse = ""),
        "\n\n# Sample name\n",
        "sample_name <- ",
        paste0("c(", paste0("'", row.names(data), "'", collapse = ","), ")", collapse = ""),
        "\n\n# Group name\n",
        "group_name <- ",
        paste0("c(", paste0("'", data.cl, "'", collapse = ","), ")", collapse = ""),
        "\n\n# Convert vector to data.frame\nlibrary(tidyr)\nlibrary(plotly)\n",
        '\nvalue_row <- strsplit(x = gsub("c\\\\(", "", data), split = "(\\\\), )|(\\\\))")',
        "\nvalue_row <- data.frame(matrix(unlist(value_row)), row.names = gene_name)",
        '\ndata <- data.frame(value_row) %>% separate(colnames(value_row), sample_name, ",")',
        "\ndata <- data.frame(lapply(data,as.numeric), row.names = gene_name)",
        "\ndata <- t(data)",
        "\n\n# Barplot\n",
        '\np <- list(0)',
        '\n\nfor (i in 1:nrow(data)) {\n',
        '    p[[i]] <- plot_ly(x = colnames(data), y = t(data[i,]), color = factor(group_name), type = "bar")\n ',
        '    p[[i]] <- p[[i]] %>% layout(annotations = list(x = 0.5, y = 1.05, text = row.names(data)[i], showarrow = F, xref = "paper", yref = "paper" ),\n',
        '                                showlegend = FALSE, xaxis = xform)\n',
        '}\n',
        'f <- list(0)\n',
        'j <- 1\n',
        'for (i in seq(1, nrow(data), 2)) {\n',
        '    if(i + 1 <= nrow(data)){\n',
        '        f[[j]] <- subplot(p[[i]], p[[i+1]])\n',
        '    } else {\n',
        '        f[[j]] <- subplot(p[[i]])\n',
        '    }\n',
        '    j <- j + 1\n',
        '}\n',
        'subplot(f, nrows = j - 1, margin = 0.05)\n'
      )
    })
    # R code of expression plot in boxplot ----
    output$expressionLevelBoxCodeText <- renderText({
      code <- paste0(
        "# Dataset\n",
        "data <- ",
        paste0("c(", paste0("'", as.character(data.frame(round(data, 2))), "'", collapse = ",\n"), ")", collapse = ""),
        "\n\n# Gene name\n",
        "gene_name <- ",
        paste0("c(", paste0("'", colnames(data), "'", collapse = ","), ")", collapse = ""),
        "\n\n# Sample name\n",
        "sample_name <- ",
        paste0("c(", paste0("'", row.names(data), "'", collapse = ","), ")", collapse = ""),
        "\n\n# Group name\n",
        "group_name <- ",
        paste0("c(", paste0("'", data.cl, "'", collapse = ","), ")", collapse = ""),
        "\n\n# Convert vector to data.frame\nlibrary(tidyr)\nlibrary(plotly)\n",
        '\nvalue_row <- strsplit(x = gsub("c\\\\(", "", data), split = "(\\\\), )|(\\\\))")',
        "\nvalue_row <- data.frame(matrix(unlist(value_row)), row.names = gene_name)",
        '\ndata <- data.frame(value_row) %>% separate(colnames(value_row), sample_name, ",")',
        "\ndata <- data.frame(lapply(data,as.numeric), row.names = gene_name)",
        "\ndata <- t(data)",
        "\n\n# Barplot\n# Reorder x label (if necessary)\n",
        '\nxOrder <- data.frame("name" = colnames(data), "group" = group_name)',
        '\nxOrderVector <- unique(xOrder[order(xOrder$group),]$name)',
        '\nxform <- list(categoryorder = "array", categoryarray = xOrderVector, title = "")',
        '\np <- list(0)',
        '\n\nfor (i in 1:nrow(data)) {\n',
        '    p[[i]] <- plot_ly(x = factor(group_name), y = t(data[i,]), color = factor(group_name), type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8)\n',
        '    p[[i]] <- p[[i]] %>% layout(annotations = list(x = 0.5, y = 1.05, text = row.names(data)[i], showarrow = F, xref = "paper", yref = "paper"), showlegend = FALSE)\n',
        '}\n',
        'f <- list(0)\n',
        'j <- 1\n',
        'for (i in seq(1, nrow(data), 2)) {\n',
        '    if(i + 1 <= nrow(data)){\n',
        '        f[[j]] <- subplot(p[[i]], p[[i+1]])\n',
        '    } else {\n',
        '        f[[j]] <- subplot(p[[i]])\n',
        '    }\n',
        '    j <- j + 1\n',
        '}\n',
        'subplot(f, nrows = j - 1, margin = 0.05)\n'
      )
    })
    # Selected gene row count DataTable ----
    output$geneTable <- DT::renderDataTable({
      df <- data
      # Create 19 breaks and 20 rgb color values ranging from white to blue
      brks <-
        quantile(df %>% select_if(is.numeric),
                 probs = seq(.05, .95, .05),
                 na.rm = TRUE)
      
      DT::datatable(df,
                    options = list(dom = "t")) %>%
        formatStyle(names(df), backgroundColor = styleInterval(brks, head(Blues(40), n = length(brks) + 1)))
    })
    

    # Selected gene TCC result DataTable -----
    output$geneTableCal <- DT::renderDataTable({
      DT::datatable(resultTable()[resultTable()$gene_id %in% row.names(data), ],
                    options = list(dom = "t")) %>% formatRound(
                      columns = c("a.value",
                                  "m.value",
                                  "p.value",
                                  "q.value"),
                      digits = 3
                    ) %>% formatStyle(
                      "estimatedDEG",
                      target = 'row',
                      backgroundColor = styleEqual(1, "lightblue")
                    )
    })
    runExp$runExpValue <- input$runExpression
})