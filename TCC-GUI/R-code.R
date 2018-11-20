# R-code.R

# TCC Runing Code ---------------------------------------------------------


observeEvent(input$TCC, {
  group <- paste(variables$groupList, collapse = ", ")
  runTCCCode <- paste0(
    '# Load library
library(TCC)
# Read Data
data <- read.table(FILE_PATH, 
                   header = TRUE, 
                   row.names = 1, 
                   sep="\\t", 
                   quote="")
  
# Grouping
data.cl <- rep(0, ncol(data))
# Function for grouping
convert2cl <- function(x, df) {
  grep(x, colnames(df))
}
group <- list(', group, ')

for (i in 1:length(group)) {
  data.cl[unlist(lapply(group[[i]], convert2cl, df = data))] = i
}
    
# Create TCC Object
tcc <- new("TCC", data, data.cl)
tcc <- filterLowCountGenes(tcc, low.count = ',input$filterLowCount, ')

# Run TCC
tcc <- calcNormFactors(tcc, 
                       norm.method = ', input$normMethod, ',
                       test.method = ', input$testMethod, ',
                       iteration = ', input$iteration, ', 
                       FDR = ', input$fdr,', 
                       floorPDEG = ', input$floorpdeg, ')
# Estimate DEGs
tcc <- estimateDE(tcc, 
                  test.method = ', input$testMethod,', 
                  FDR = ',input$fdr, ')
    
# Save the result
result <- getResult(tcc, 
                    sort = FALSE)
    
# Also you can get the normalized data
normalizedData <- tcc$getNormalizedData()
')
  variables$runTCCCode <- runTCCCode
})


# MA Plot Runing Code -----------------------------------------------------


observeEvent(input$makeMAPlot, {
  runMAPlot <- paste0(
    '# Make sure the code run after [TCC Calculation code]
library("plotly")
plot_ly(data = result, 
           x = ~a.value,
           y = ~m.value, 
        type = "scatter", 
        mode = "markers", 
       color = ~x, 
      colors = c(',input$fdrColor,', "#000000"), 
      marker = list(size = 3), 
   hoverinfo = "text", 
        text = ~paste("Gene:", 
                      result[, ',input$GeneAttribute,'], 
                      "A value:", round(a.value, 4), 
                      "M value:", round(m.value, 4), 
                      "Rank:", rank)
                      ) %>%
  layout(xaxis = list(title = "A = (log2(G2)+log2(G1))/2"), 
         yaxis = list(title = "M = log2(G2)-log2(G1)"), 
         title = paste("MA Plot with FDR <", ', input$maFDR,')'
    )
  variables$runMAPlot <- runMAPlot
})


# Volcano Plot Runing Code ------------------------------------------------


observeEvent(input$makeVolcanoPlot, {
  runVolcanoPlot <- paste0(
    '# Make sure the code run after [TCC Calculation code]
library("plotly")
dt <- result

# Separate up-down group
downCut <- ', input$CutFC[1],'
upCut <- ', input$CutFC[2],'

pCut <- log2(as.numeric(',input$Cutpvalue,'))

dt$color <- ""
dt[dt$m.value <= downCut, ]$color <- "Down"
dt[dt$m.value >= upCut, ]$color <- "Up"
dt[dt$p.value > as.numeric(',input$Cutpvalue,'), ]$color <- "None"
dt[dt$m.value <= upCut & dt$m.value >= downCut, ]$color <- "None"

x <- factor(dt$color)
levels(x) <- list("Down" = 0, "None" = 1, "Up" = 2)
    
# Make plot
plot_ly(data = dt, 
           x = ~ m.value, 
           y = ~ -log10(p.value), 
        type = "scatter",  
        mode = "markers", 
       color = ~ x, 
      colors = c(',input$downColor,', "black", ',input$upColor,'), 
      marker = list(size = 3), 
   hoverinfo = "text", 
        text =~ paste("Gene:", result[, ', input$GeneAttribute,'], 
                      "A value:", round(a.value, 4), 
                      "M value:", round(m.value, 4),  
                      "p-value:", round(p.value, 4), 
                      "q-value:", round(q.value, 4), 
                      "Rank:", rank)) %>%
            layout(xaxis = list(title = ',input$xlabs,'), 
                   yaxis = list(title = ',input$ylabs,'), 
                   title = ', input$graphicTitle,',
                  shapes = list(list(type = "line", 
                                       y0 =~ min(-log10(p.value)), 
                                       y1 =  ~ max(-log10(p.value)), 
                                       x0 = upCut, x1 = upCut, 
                                     line = list(dash = "dot", width = 2)
                                    ),
                                list(type = "line", 
                                       y0 =~ min(-log10(p.value)), 
                                       y1 =~ max(-log10(p.value)), 
                                       x0 = downCut, 
                                       x1 = downCut, 
                                     line = list(dash = "dot", width = 2)),
                                list(type = "line", 
                                       y0 = -log10(as.numeric(',input$Cutpvalue,')), 
                                       y1 = -log10(as.numeric(',input$Cutpvalue,')), 
                                       x0 =~ min(m.value), 
                                       x1 =~ max(m.value), 
                                     line = list(dash = "dot", width = 2))
                                )
                   )')
  variables$runVolcanoPlot <- runVolcanoPlot
})


# PCA Plot Runing Code ----------------------------------------------------


observeEvent(input$pcRun, {
  group <- paste(variables$groupList, collapse = ", ")
  runPCACode <- paste0(
'data <- read.table(FILE_PATH, 
                   header = TRUE, 
                row.names = 1, 
                      sep = "\\t", 
                    quote = "")

# Grouping
data.cl <- rep(0, ncol(data))
# Function for grouping
convert2cl <- function(x, df) {
  grep(x, colnames(df))
}
group <- list(', group, ')
  
for (i in 1:length(group)) {
  data.cl[unlist(lapply(group[[i]], convert2cl, df = data))] = i
}

# Using Original Dataset.
data <- data[data.cl != 0]

data.cl <- data.cl[data.cl != 0]

# Select DEGs (Row)
data <- data[result$q.value <= ', input$pcFDR, ', ]

# PCA processing tranform
data <- t(log(data + 1))
data.pca <- prcomp(data[ , apply(data, 2, var) != 0],
                     center = ', input$pcCenter,',
                     scale. = ', input$pcScale, ') 
')
  variables$runPCACode <- runPCACode

})


# Heatmap Plot Runing Code ------------------------------------------------


observeEvent(input$heatmapRun, {
  group <- paste(variables$groupList, collapse = ", ")
  runHeatmapCode <- paste0('
# Select Sample (Column)
# Grouping
data.cl <- rep(0, ncol(data))
# Function for grouping
convert2cl <- function(x, df) {
  grep(x, colnames(df))
}

group <- list(',group,')
for (i in 1:length(group)) {
  data.cl[unlist(lapply(group[[i]], convert2cl, df = data))] = i
}

# Using Original Dataset or Normalized Dataset.
if (', input$heatmapData, ' == "o") {
  data <- data[data.cl != 0]
} else {
  data <- normalizedData
}
data.cl <- data.cl[data.cl != 0]

# Select DEGs (Row)
if (', input$heatmapGeneSelectType, ' == "Paste a list of genes") {
  data <-
    data[row.names(data) %in% unlist(strsplit(x = ', input$heatmapTextList, ', split = "[\r\n]")), ]
}
if (', input$heatmapGeneSelectType, ' == "Select genes by name") {
  data <- data[row.names(data) %in% ', input$heatmapSelectList, ', ]
}
if (', input$heatmapGeneSelectType, ' == "Select genes according FDR") {
  if (', input$testMethod, ' == "wad") {
    data <-
      data[row.names(data) %in% result[result$rank <= ', input$heatmapFDRTop, ',]$gene_id,]
  } else {
    data <-
      data[row.names(data) %in% result[result$rank <= ', input$heatmapFDRTop, ' &
                     result$q.value <= ', input$heatmapFDR, ',]$gene_id,]
  }
}

# Create Plotly object
  heatmaply(
    t(data),
    k_row = length(group),
    colors = RdYlGn,
    dist_method = ', input$heatmapDist, ',
    hclust_method = ', input$heatmapCluster, ',
    xlab = "Gene",
    ylab = "Sample",
    main = paste0(
      "Heatmap of gene expression (FDR < ",
      ', input$heatmapFDR, ',
      ", ",
      dim(data)[1],
      "DEGs)"
    ),
    margins = c(150, 100, 40, 20),
    scale = ', input$heatmapScale, ',
    labCol = colnames(t(data)),
    labRow = row.names(t(data))
  )')
  variables$runHeatmap <- runHeatmapCode
})


# Expression Plot Runing Code ---------------------------------------------


