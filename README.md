# TCC-GUI

**TCC: Differential expression analysis for tag count data with robust normalization strategies**

 [Full Text](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3716788/)

![TCC LOGO](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/tccLogo.png)

This package provides a series of functions for performing differential expression analysis from RNA-seq count data using robust normalization strategy (called DEGES). The basic idea of DEGES is that potential differentially expressed genes or transcripts (DEGs) among compared samples should be removed before data normalization to obtain a well-ranked gene list where true DEGs are top-ranked and non-DEGs are bottom ranked. This can be done by performing a multi-step normalization strategy (called DEGES for DEG elimination strategy). A major characteristic of TCC is to provide the robust normalization methods for several kinds of count data (two-group with or without replicates, multi-group/multi-factor, and so on) by virtue of the use of combinations of functions in depended packages.

Author: Jianqiang Sun, Tomoaki Nishiyama, Kentaro Shimizu, and Koji Kadota

GUI Version Developer: Wei Su



---

### 0. Installation

Run the following command in your **RStudio**.

```R
libs <- c("shinythemes", "shinyalert", "plotly", "colourpicker", "dplyr", "TCC", "DT", "heatmaply", "data.table", "colourpicker", "markdown",
"plotlyBars")

for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}

shiny::runGitHub("TCC-GUI", "swsoyee", subdir = "TCC-GUI")
```

(The installation command hasn't been test, please install those package by yourself if any error occurred. The online version isn't the latest one. Local version is highly recommended now.)

### <a name="Datainput"></a> 1. Data input

---
- Click **"Computation"** tab on the top;

- Click **"Load Sample Data"** button for test or Click **"Upload..."** button if you want to used your own count dataset.

  ![Raw Count Table](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/RawCountTable_.png)

### <a name="Computation"></a> 2. Computation

---
- Input your group info in **Group Selection** section and click **"Confirmed"**, then the **"TCC Parameters"** Panel will show up;

  <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/GroupSelection_.png" width="359" height="325"> 

- Change the parameters for computation (if you wish), and click the **"Run TCC"**.

  <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/TCC_.png" width="359" height="697">

- After computation, **"Result Table"**, and series tab for other analysis will show up.

  ![Result Table](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/ResultTable_.png)

### <a name="MAplot"></a> 3.1 MA plot

---
- After computation, switch to **"MA Plot"** tab, and click **"Generate MA-Plot"** button;

- Hover cursor on the point, and the additional information will be provided (Gene expression plot).

  ![MA Plot](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/maplot1-md_.png)

- If you want to mark some gene on the plot, please click the specific rows of gene, and click **"Generate MA-Plot" **button again to refresh the plot.

  ![MA Plot Marker](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/maplot2-md_.png)

- (This is prerelease version. more functions need to be add in)

### <a name="Volcanoplot"></a> 3.2 Volcano plot

---
- Same as part of **MA Plot**.
 ![volcanoplot](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/volcanoplot_.png)
- (This is prerelease version. more functions need to be add in)

### <a name="PCAanalysis"></a> 3.3 PCA analysis

---
- Change the parameters for PCA (if you wish), and click the **"Run"** button.

  ![pca](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/pca_.png)

- (This is prerelease version. more functions need to be add in)

### <a name="Heatmap"></a> 3.4 Heatmap

---
- Change the parameters for heatmap (if you wish), and click the **"Run"** button.

  ![heatmap](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/heatmap_.png)

- (This is prerelease version. more functions need to be add in)

### <a name="Expression"></a> 3.5 Expression

---
- Select gene(s) you want to plot in the left panel. **Barplot** and **Boxplot** are provided.
   ![expression](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/expressionPlot_.png)

- (This is prerelease version. more functions need to be add in)

    

### 3.6  Report output
---
- Generate report in `Markdown`, `HTML` or `Word` (This function have not been finished). 

### <a name="Morehelps"></a> 4. More helps

---

- [Install Original TCC package from Bioconductor](http://www.bioconductor.org/packages/release/bioc/html/TCC.html)
