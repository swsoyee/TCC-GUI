# server-volcano-plot.R

# 如果运行了TCC之后，方法不是'WAD'的话，则有pvalue和qvalue值，可绘制火山图
observeEvent(input$TCC, {
  # output$resultTableInVolcanalPlot <- output$resultTableInPlot
  
  if (input$testMethod != "wad") {
    output$valcanoParameter <- renderUI({
      tagList(
        selectInput(
          "GeneAttributeVol",
          "Hover info：",
          choices = colnames(resultTable())
        ),
        tags$hr(),
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
        actionButton("makeVolcanoPlot", "Generate Volcano Plot")
      )
    })
  } else {
    output$valcanoParameter <- renderUI({
      tagList(
        tags$p("Because you have chosen \"WAD\" test method, there is no p-value and q-value result for plotting.")
      )
    })
  }
})

# 点击了绘图按钮后进行绘制
observeEvent(input$makeVolcanoPlot, {
  output$volcanoPloty <- renderPlotly({
    
    validate(
      need(resultTable()$p.value != "", "No p-values for ploting.")
    )
    
    req(input$makeVolcanoPlot)
    isolate({
      # 数据集整理
      dt <- resultTable()
      
      downCut <- input$CutFC[1]
      upCut <- input$CutFC[2]
      pCut <- log2(as.numeric(input$Cutpvalue))
      
      dt$color <- ""
      dt[dt$m.value <= downCut, ]$color <- "Down"
      dt[dt$m.value >= upCut, ]$color <- "Up"
      dt[dt$p.value > as.numeric(input$Cutpvalue), ]$color <- "None"
      dt[dt$m.value <= upCut & dt$m.value >= downCut, ]$color <- "None"
      
      x <- factor(dt$color)
      levels(x) <- list("Down"=0, "None"=1, "Up"=2)  #LegendのLevelをリネームする
      
      # 添加注释
      key <- row.names(dt)
      
      # print(head(dt, n=10))
      
      if(is.null(input$resultTableInVolcanalPlot_rows_selected)) {
        annotation <- list()
      } else {
        markerSelect <- dt[input$resultTableInVolcanalPlot_rows_selected,]
        
        annotation <- list(
          x = markerSelect$m.value,
          y = -log10(markerSelect$p.value),
          text = markerSelect[, input$GeneAttribute],
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 20,
          ay = 40
        )
      }
      
      plot_ly(data = dt,
              x = ~m.value,
              y = ~-log10(p.value),
              type = "scatter",
              mode = "markers",
              color = ~x,
              colors = c("green", "black", "red"),
              marker = list(size = 3),
              hoverinfo = "text",
              text = ~paste("</br>Gene:", resultTable()[, input$GeneAttribute],
                            "</br>A value:", round(a.value, 4),
                            "</br>M value:", round(m.value, 4),
                            "</br>p-value:", round(p.value, 4),
                            "</br>q-value:", round(q.value, 4),
                            "</br>Rank:", rank),
              source = "volcano") %>%
        layout(xaxis = list(title = input$xlabs),
               yaxis = list(title = input$ylabs),
               title = input$graphicTitle,
               annotations = annotation,
               shapes= list(list(type='line', 
                                 y0=~min(-log10(p.value)),
                                 y1=~max(-log10(p.value)), 
                                 x0= upCut, 
                                 x1= upCut,
                                 line=list(dash='dot', width=2)),
                            list(type='line', 
                                 y0=~min(-log10(p.value)),
                                 y1=~max(-log10(p.value)), 
                                 x0= downCut, 
                                 x1= downCut,
                                 line=list(dash='dot', width=2)),
                            list(type='line', 
                                 y0= -log10(as.numeric(input$Cutpvalue)),
                                 y1= -log10(as.numeric(input$Cutpvalue)), 
                                 x0=~min(m.value), 
                                 x1=~max(m.value),
                                 line=list(dash='dot', width=2))))
    })
  })
})


output$geneBarPlotInVolcano <- renderPlotly({
  # Read in hover data
  eventdata <- event_data("plotly_hover", source = "volcano")
  validate(need(!is.null(eventdata), 
                "Hover over the point to show original expression plot"))
  # Get point number
  datapoint <- as.numeric(eventdata$pointNumber)[1]
  # Get expression level (Original)
  expression <- variables$CountData[datapoint, ]
  # Get expression level (Normalized)
  expressionNor <- t(t(variables$norData[datapoint, ]))

  
  data <- variables$CountData
  data.cl <- rep(0, ncol(data))
  convert2cl <- function(x, df) {
    grep(x, colnames(df))
  }
  for (i in 1:length(variables$groupList)) {
    data.cl[unlist(lapply(variables$groupList[[i]], convert2cl, df = data))] = i
  }
  
  expression <- t(expression[data.cl != 0])
  data.cl <- data.cl[data.cl != 0]
  
  plot_ly(x = ~row.names(expression),
          y = ~expression[, 1],
          color = as.factor(data.cl),
          text = expression[, 1], 
          textposition = 'auto',
          type = "bar",
          name = "Original") %>%
    add_trace(y = ~expressionNor[, 1],
              text = round(expressionNor[, 1], 2), 
              textposition = 'auto',
              name = "Normalized",
              type = "scatter",
              mode = "lines+markers",
              marker = list(size = 10,
                            line = list(color = 'rgba(0, 0, 0, 0)',
                                        width = 2))) %>%
    layout(xaxis = list(title = "Sample Name"),
           yaxis = list(title = "Raw Count"),
           title = "Expression Plot",
           legend = list(orientation = 'h'))
})