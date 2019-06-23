<p align="center">
    <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/tccLogo.png" width="121" height="60">  
</p>
<p align="center">📊 <b>TCC-GUI: Graphical User Interface for TCC package</b></p>
<p align="center">
    <img src="https://img.shields.io/github/last-commit/swsoyee/TCC-GUI.svg">
    <img src="https://img.shields.io/github/license/swsoyee/TCC-GUI.svg">
</p>

---
<img src="https://github.com/swsoyee/TCC-GUI/blob/master/ScreenShot/Home.png" width="420" align="right" style="max-width: 50%">

[`TCC`](http://bioconductor.org/packages/TCC/)<sup>1</sup> is a [`R`](https://www.r-project.org/)/[`Bioconductor`](https://www.bioconductor.org/) package provides a series of functions for performing differential expression  (**DE**)  analysis from    RNA-seq count data using a robust normalization strategy (called **DEGES**).  

The basic idea of **DEGES** is that potential differentially expressed genes (**DEGs**) among compared samples should be removed before data normalization to obtain a well-ranked gene list where true **DEGs** are top-ranked and **non-DEGs** are bottom ranked. This can be done by performing the multi-step normalization procedures based on **DEGES** (**DEG elimination strategy**) implemented in TCC.    

TCC internally uses functions provided by [`edgeR`](https://www.bioconductor.org/packages/release/bioc/html/edgeR.html)<sup>2</sup>, [`DESeq`](https://www.bioconductor.org/packages/release/bioc/html/DESeq.html)<sup>3</sup>, [`DESeq2`](https://www.bioconductor.org/packages/release/bioc/html/DESeq2.html)<sup>4</sup>, and [`baySeq`](https://www.bioconductor.org/packages/release/bioc/html/baySeq.html)<sup>5</sup> . The multi-step normalization of TCC can be done by using functions in the four packages.   

In this `GUI version of TCC (TCC-GUI)`, all parameter settings are available just like you are using the original one. Besides, it also provides lots of plotting functions where the original package is unsupported now.   


```
Tips: Development is now undergoing, some functions and features may be changed in the final version.
```



## 📈 Features

| Simulation Data Generation                                | Exploratory Analysis                                      |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta1.png"> | <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta2.png"> |
| <p align="center">**TCC Computation**</p>                                  | <p align="center">**MA Plot Generation**</p>                                    |
| <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta3.png"> | <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta4.png"> |
| <p align="center">**Volcano Plot Generation**</p>                               | <p align="center">**Heatmap Generation**</p>                                    |
| <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta5.png"> | <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta7.png"> |
| <p align="center">**Expression Level Plot Generation**</p>                      | <p align="center">**Report Generation**</p>                                     |
| <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta8.png"> | <img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta9.png"> |

## 📔 Usage

### 🌐 Online version  
Go to 🔗[`TCC-GUI`](https://infinityloop.shinyapps.io/TCC-GUI/).  

### 💻 Standalone version  
<details>
<summary><b>📲 Installation</b></summary>  
    
---------

Make sure that you have already installed those packages in your environment.   

`shiny`, `shinydashboard`, `shinyWidgets`, `plotly`, `dplyr`, `TCC`, `DT`, `heatmaply`,  `rmarkdown`, `data.table`, `tidyr`, `RColorBrewer`, `utils`, `knitr`, `cluster`, `shinycssloaders`, `shinyBS`, `MASS`.    

If any package is missing, Please run the following command in your [`RStudio`](https://www.rstudio.com/) and it will install all packages automatically.  

```R
# Check "BiocManager"
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# Package list
libs <- c("shiny", "shinydashboard", "shinyWidgets", "plotly", "dplyr", "DT", "heatmaply", "tidyr","utils","rmarkdown","data.table","RColorBrewer", "knitr", "cluster", "shinycssloaders", "shinyBS", "MASS", "TCC")

# Install packages if missing
for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
     BiocManager::install(i, suppressUpdates=TRUE)
  }
}
```
</details>

<details>  
<summary><b>⭕ Launch</b></summary>  
    
---------

Run the following command to launch `TCC-GUI` in your local environment, then it will download `TCC-GUI` automatically from github and launch.  

##### Method 1  
```R
shiny::runGitHub("TCC-GUI", "swsoyee", subdir = "TCC-GUI", launch.browser = TRUE)
```

This method always download the source code from github before launching, so maybe you can try to download all the source code by yourself and launch it.   

##### Method 2  
1. Click `Clone or download` button on the top of this page, then click [`Download ZIP`](https://github.com/swsoyee/TCC-GUI/archive/master.zip);  
2. Unzip the file to your working directory (use `getwd()` to know your working directory);  
3. Run the code of launching (according to your structure of working directory it may be different).   

  ```R
  shiny::runApp("TCC-GUI-master//TCC-GUI", launch.browser = TRUE)
  ```

</details>

## 📕 Publication
> **TCC-GUI: a Shiny-based application for differential expression analysis of RNA-Seq count data**  
Wei Su, Jianqiang Sun, Kentaro Shimizu and Koji Kadota  
*BMC Research Notes* 2019 **12**:133  
https://doi.org/10.1186/s13104-019-4179-2 | © The Author(s) 2019  
**Received:** 14 January 2019 | **Accepted:** 11 March 2019 | **Published:** 13 March 2019

## 📚 References

[1] Sun J, Nishiyama T, Shimizu K, et al. **TCC**: an R package for comparing tag count data with robust normalization strategies. *BMC bioinformatics*, 2013, 14(1): 219.  

[2] Robinson M D, McCarthy D J, Smyth G K. **edgeR**: a Bioconductor package for differential expression analysis of digital gene expression data. *Bioinformatics*, 2010, 26(1): 139-140.  

[3] Anders S, Huber W. Differential expression analysis for sequence count data. *Genome biology*, 2010, 11(10): R106.   

[4] Love M I, Huber W, Anders S. Moderated estimation of fold change and dispersion for RNA-seq data with **DESeq2**. *Genome biology*, 2014, 15(12): 550.  

[5] Hardcastle T J, Kelly K A. **baySeq** : empirical Bayesian methods for identifying differential expression in sequence count data. *BMC bioinformatics*, 2010, 11(1): 422.  
