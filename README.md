# TCC-GUI

**TCC: Differential expression analysis for tag count data with robust normalization strategies**

 [Full Text](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3716788/)

<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/tccLogo.png" width="121" height="60">

This package provides a series of functions for performing differential expression analysis from RNA-seq count data using robust normalization strategy (called DEGES). The basic idea of DEGES is that potential differentially expressed genes or transcripts (DEGs) among compared samples should be removed before data normalization to obtain a well-ranked gene list where true DEGs are top-ranked and non-DEGs are bottom ranked. This can be done by performing a multi-step normalization strategy (called DEGES for DEG elimination strategy). A major characteristic of TCC is to provide the robust normalization methods for several kinds of count data (two-group with or without replicates, multi-group/multi-factor, and so on) by virtue of the use of combinations of functions in depended packages.

Author: Jianqiang Sun, Tomoaki Nishiyama, Kentaro Shimizu, and Koji Kadota

**GUI Version Developer: Wei Su**



---

**Note: Development is now undergoing, some function may be changed in the final version.**

---



### Online version

Go to [TCC-GUI](https://infinityloop.shinyapps.io/TCC-GUI/).

### Local version

Make sure that you have already installed those packages in your environment.

`shiny`, `shinydashboard`, `shinyWidgets`, `plotly`, `dplyr`, `TCC`, `DT`, `heatmaply`, `plotlyBars`, `markdown`, `data.table`, `tidyr`, `RColorBrewer`, `utils`.

If any package is missing, Please run the following command in your **RStudio** and it will install all packages automatically.

```R
# Part1. Install via CRAN
libs <- c("shiny",
          "shinydashboard", 
          "shinyWidgets", 
          "plotly", 
          "dplyr", 
          "DT", 
          "heatmaply",
          "tidyr",
          "devtools",
          "utils",
          "markdown",
          "data.table",
          "RColorBrewer")

for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
}

# Part2. Install via Bioconductor
if( !is.element("TCC", .packages(all.available = TRUE)) ) {
    ## try http:// if https:// URLs are not supported
    source("https://bioconductor.org/biocLite.R")
    biocLite("TCC")
}

# Part3. Install via Github
if( !is.element("plotlyBars", .packages(all.available = TRUE)) ) {
    devtools::install_github("andrewsali/plotlyBars")
}
```

Run the following command to launch `TCC-GUI` in your local environment.

```R
shiny::runGitHub("TCC-GUI", "swsoyee", subdir = "TCC-GUI")
```


<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/dataImport.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/calculation.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/MAPlot.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/VolcanoPlot.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/PCA.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/Heatmap.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/MAPlot.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/Expression.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/report.png">
