# R-code.R

#### TCC Runing Code
observeEvent(input$TCC, {
  group <- paste(variables$groupList, collapse = ", ")
  runTCCCode <- paste0(
    '<p style="text-align: left;">
    <font color="red"># Load library</font>
    <br>library(TCC)
    <br><font color="red"># Read Data</font>
    <br>data <- read.table(FILE_PATH, header = TRUE, row.names = 1, sep="\t", quote="")
    <br>
    <br><font color="red"># Grouping</font>
    <br>data.cl <- rep(0, ncol(data))
    <br><font color="red"># Function for grouping</font>
    <br>convert2cl <- function(x, df) {
    <br>&nbsp;&nbsp;grep(x, colnames(df))
    <br>}
    <br>
    <br>group <- list(',
    group,
    ')
    <br>for (i in 1:length(group)) {
    <br>&nbsp;&nbsp;data.cl[unlist(lapply(group[[i]], convert2cl, df = data))] = i
    <br>}
    <br>
    <br><font color="red"># Create TCC Object</font>
    <br>tcc <- new("TCC", data, data.cl)
    <br>tcc <- filterLowCountGenes(tcc, low.count = ',input$filterLowCount, ')
    <br><font color="red"># Run TCC</font>
    <br>tcc <- calcNormFactors(tcc, norm.method = ',
    input$normMethod,
    ', test.method = ',
    input$testMethod,
    ',iteration = ',
    input$iteration,
    ', FDR = ',
    input$fdr,
    ', floorPDEG = ',
    input$floorpdeg,
    ')
    <br><font color="red"># Estimate DEGs</font>
    <br>tcc <- estimateDE(tcc, test.method = ',
    input$testMethod,
    ', FDR = ',
    input$fdr,
    ')
    <br>
    <br><font color="red"># Save the result</font>
    <br>result <- getResult(tcc, sort = FALSE)
    <br>
    <br><font color="red"># Also you can get the normalized data</font>
    <br>normalizedData <- tcc$getNormalizedData()
    </p">
    '
    )
  variables$runTCCCode <- runTCCCode
})

#### MA Plot Runing Code
observeEvent(input$makeMAPlot, {
  runMAPlot <- paste0(
    '
    <p style="text-align: left;">
    <br>plot_ly(data = result, x = ~a.value, y = ~m.value, type = "scatter", mode = "markers", color = ~x, colors = c(',
    input$fdrColor,
    ', "#000000"), marker = list(size = 3), hoverinfo = "text", text = ~paste("Gene:", result[, ',
    input$GeneAttribute,
    '], "A value:", round(a.value, 4), "M value:", round(m.value, 4), "Rank:", rank)) %>%
    <br>          layout(xaxis = list(title = "A = (log2(G2)+log2(G1))/2"), yaxis = list(title = "M = log2(G2)-log2(G1)"), title = paste("MA Plot with FDR <", ',
    input$maFDR,
    ')'
    )
  variables$runMAPlot <- runMAPlot
})

#### Volcano Plot Runing Code
observeEvent(input$makeVolcanoPlot, {
  runVolcanoPlot <- paste0(
    '
    <p style="text-align: left;">
    <br>dt <- result
    <br>
    <br><font color="red"># Separate up-down group</font>
    <br>downCut <- ',
    input$CutFC[1],
    '
    <br>upCut <- ',
    input$CutFC[2],
    '
    <br>pCut <- log2(as.numeric(',
    input$Cutpvalue,
    '))
    <br>
    <br>dt$color <- ""
    <br>dt[dt$m.value <= downCut, ]$color <- "Down"
    <br>dt[dt$m.value >= upCut, ]$color <- "Up"
    <br>dt[dt$p.value > as.numeric(',
    input$Cutpvalue,
    '), ]$color <- "None"
    <br>dt[dt$m.value <= upCut & dt$m.value >= downCut, ]$color <- "None"
    <br>
    <br>x <- factor(dt$color)
    <br>levels(x) <- list("Down" = 0, "None" = 1, "Up" = 2)
    <br>
    <br><font color="red"># Make plot</font>
    <br>plot_ly( data = dt, x = ~ m.value, y = ~ -log10(p.value), type = "scatter",  mode = "markers", color = ~ x, colors = c(',
    input$downColor,
    ', "black", ',
    input$upColor,
    '), marker = list(size = 3), hoverinfo = "text", text = ~ paste( "Gene:", result[, ',
    input$GeneAttribute,
    '], "A value:", round(a.value, 4), "M value:", round(m.value, 4),  "p-value:", round(p.value, 4), "q-value:", round(q.value, 4), "Rank:", rank)) %>%
    <br>        layout(xaxis = list(title = ',
    input$xlabs,
    '), yaxis = list(title = ',
    input$ylabs,
    '), title = ',
    input$graphicTitle,
    ',
    <br>shapes = list(
    <br>            list( type = "line", y0 =  ~ min(-log10(p.value)), y1 =  ~ max(-log10(p.value)), x0 = upCut, x1 = upCut, line = list(dash = "dot", width = 2)),
    <br>            list(type = "line", y0 =  ~ min(-log10(p.value)), y1 =  ~ max(-log10(p.value)), x0 = downCut, x1 = downCut, line = list(dash = "dot", width = 2)),
    <br>            list( type = "line", y0 = -log10(as.numeric(',
    input$Cutpvalue,
    ')), y1 = -log10(as.numeric(',
    input$Cutpvalue,
    ')), x0 =  ~ min(m.value), x1 =  ~ max(m.value), line = list(dash = "dot", width = 2))))
    '
    )
  variables$runVolcanoPlot <- runVolcanoPlot
})

#### PCA Plot Runing Code


#### Heatmap Plot Runing Code


#### Expression Plot Runing Code