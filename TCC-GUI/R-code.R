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
<br>group <- list(',group, ')
<br>for (i in 1:length(group)) {
<br>&nbsp;&nbsp;data.cl[unlist(lapply(group[[i]], convert2cl, df = data))] = i
<br>}
<br>
<br><font color="red"># Create TCC Object</font>
<br>tcc <- new("TCC", data, data.cl)
<br><font color="red"># Run TCC</font>
<br>tcc <- calcNormFactors(tcc, norm.method = ', input$normMethod,', test.method = ', input$testMethod,',iteration = ', input$iteration,', FDR = ', input$fdr,', floorPDEG = ', input$floorpdeg,')
<br><font color="red"># Estimate DEGs</font>
<br>tcc <- estimateDE(tcc, test.method = ', input$testMethod,', FDR = ', input$fdr,')
<br>
<br><font color="red"># Save the result</font>
<br>result <- getResult(tcc, sort = FALSE)
<br>
<br><font color="red"># Also you can get the normalized data</font>
<br>normalizedData <- tcc$getNormalizedData()
</p">
')
  variables$runTCCCode <- runTCCCode
})