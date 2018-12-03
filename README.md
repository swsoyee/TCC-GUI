

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

## 📈 Screenshot
(Screenshots are not to the date!)
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/Home.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/Simulation.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/dataImport.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/calculation.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/MAPlot.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/VolcanoPlot.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/PCA.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/Heatmap.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/MAPlot.png">
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/Expression.png">

This function is under developing now...
<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/report.png">

## 🌐 Online version

Go to 🔗[TCC-GUI](https://infinityloop.shinyapps.io/TCC-GUI/).

## 💻 Local version

### 📥 Installation
Make sure that you have already installed those packages in your environment.

`shiny`, `shinydashboard`, `shinyWidgets`, `plotly`, `dplyr`, `TCC`, `DT`, `heatmaply`,  `rmarkdown`, `data.table`, `tidyr`, `RColorBrewer`, `utils`, `knitr`, `cluster`, `shinycssloaders`, `shinyBS`，`rclipboard`.

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
          "utils",
          "rmarkdown",
          "data.table",
          "RColorBrewer",
          "knitr",
          "cluster",
          "shinycssloaders",
          "shinyBS",
          "rclipboard")

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
```

### ▶ Launch
Run the following command to launch `TCC-GUI` in your local environment, then it will download `TCC-GUI` automatically from github and launch.

#### Method 1

```R
shiny::runGitHub("TCC-GUI", "swsoyee", subdir = "TCC-GUI", launch.browser = TRUE)
```

This method always download the source code from github before launching, so maybe you can try to download all the source code by yourself and launch it. 

#### Method 2

1. Click `Clone or download` button on the top of this page, then click `Download ZIP`;

2. Unzip the file to your working directory ( use `getwd()` to know your working directory );

3. Run the code of launching ( according to your structure of working directory it may be different ). 

   ```R
   shiny::runApp("TCC-GUI-master//TCC-GUI", launch.browser = TRUE)
   ```
4. Enjoy your analysis on `TCC-GUI`!



> 🔗Emoji icons supplied by [EmojiOne](https://www.emojione.com/)