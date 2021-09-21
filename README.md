# 📊 TCC-GUI: Graphical User Interface for TCC package

<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/TCC-GUI/www/tcc-gui-logo.png" width="120" align="right">  

![last commit](https://img.shields.io/github/last-commit/swsoyee/TCC-GUI.svg)
![license](https://img.shields.io/github/license/swsoyee/TCC-GUI.svg)
[![citations](https://img.shields.io/badge/citations-28-blue?link=https://scholar.google.com/scholar?rlz=1C5CHFA_enJP843JP843&um=1&ie=UTF-8&lr&cites=4813951894701221269)](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=4813951894701221269&as_sdt=5)

[`TCC`](http://bioconductor.org/packages/TCC/)<sup>1</sup> is a [`R`](https://www.r-project.org/)/[`Bioconductor`](https://www.bioconductor.org/) package provides a series of functions for performing differential expression  (**DE**)  analysis from    RNA-seq count data using a robust normalization strategy (called **DEGES**).  

The basic idea of **DEGES** is that potential differentially expressed genes (**DEGs**) among compared samples should be removed before data normalization to obtain a well-ranked gene list where true **DEGs** are top-ranked and **non-DEGs** are bottom ranked. This can be done by performing the multi-step normalization procedures based on **DEGES** (**DEG elimination strategy**) implemented in TCC.  

<img src="https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/Home.png" width="250" align="left">

TCC internally uses functions provided by [`edgeR`](https://www.bioconductor.org/packages/release/bioc/html/edgeR.html)<sup>2</sup>, [`DESeq2`](https://www.bioconductor.org/packages/release/bioc/html/DESeq2.html)<sup>3</sup>, and [`baySeq`](https://www.bioconductor.org/packages/release/bioc/html/baySeq.html)<sup>4</sup> . The multi-step normalization of TCC can be done by using functions in the four packages.  

In this `GUI version of TCC (TCC-GUI)`, all parameter settings are available just like you are using the original one. Besides, it also provides lots of plotting functions where the original package is unsupported now.  

> Tips: Development is now undergoing, some functions and features may be changed in the final version.

## 📈 Features

| Simulation Data Generation| Exploratory Analysis|
| --- | --- |
| ![Simulation Data Generation](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta1.png) | ![Exploratory Analysis](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta2.png) |
| <div align="center">**TCC Computation**</div>| <div align="center">**MA Plot Generation**</div>|
| ![TCC Computation](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta3.png) | ![MA Plot Generation](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta4.png) |
| <div align="center">**Volcano Plot Generation**</div>| <div align="center">**Heatmap Generation**</div>|
| ![Volcano Plot Generation](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta5.png) | ![Heatmap Generation](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta7.png) |
| <div align="center">**Expression Level Plot Generation**</div>| <div align="center">**Report Generation**</div>|
| ![Expression Level Plot Generation](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta8.png) | ![Report Generation](https://raw.githubusercontent.com/swsoyee/TCC-GUI/master/ScreenShot/beta9.png) |

## 📔 Usage

### Online version

Access [`TCC-GUI`](https://infinityloop.shinyapps.io/TCC-GUI/) hosted by shinyapps.io. Due to the limitations of the free version of shinyapps, you may not be able to use the tool in some cases, in which case you may consider downloading the source code and launch the tool in a your machine (see below).  

### Standalone version  

If you are familiar with git, **Method 1** is highly recommended.

### Method 1

1. Use the command below to clone the source code to your local directory. We assume you already know how to clone a project using Git from the command line, if not please refer to [Git Basics - Getting a Git Repository](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository).

    ```bash
    git clone https://github.com/swsoyee/TCC-GUI.git ~/Desktop/TCC-GUI
    ```

2. When you open this project (just double click `TCC-GUI.Rproj`) in R at first time, the following message will be print in console, and the package `renv` will be install automatically (if not, please install `renv` manually or [create a issue for help](https://github.com/swsoyee/TCC-GUI/issues/new/choose)). Next, use `renv::restore()` to install all other packages which are needed.

    ```r
    # Bootstrapping renv 0.12.5 --------------------------------------------------
    * Downloading renv 0.12.5 from CRAN ... OK
    * Installing renv 0.12.5 ... Done!
    * Successfully installed and loaded renv 0.12.5.
    * Project '~/Desktop/TCC-GUI' loaded. [renv 0.12.5]
    * The project library is out of sync with the lockfile.
    * Use `renv::restore()` to install packages recorded in the lockfile.
    
    > renv::restore()
    The following package(s) will be updated:
    ...
    ````

3. If you are using `RStudio`, just open the `ui.R`, `server.R` or `global.R` in `TCC-GUI` directory, and click the `Run App` button to launch the application. Or use the commend below to complete the same thing.

    ```r
    shiny::runApp(appDir = "TCC-GUI/")
    ```

### Method 2

1. Click `Clone or download` button on the top of this page, then click [`Download ZIP`](https://github.com/swsoyee/TCC-GUI/archive/master.zip);  
2. Unzip the file to your working directory;
3. Double click `TCC-GUI.Rproj` to open the project;
4. Make sure the `renv` package is install automatically (also see Method 1 step 2);
4. Run the code to launch the application (according to your structure of working directory it may be different).  

    ```R
    # install packages by using renv
    renv::restore()

    # run the command and launch the application
    shiny::runApp(appDir = "TCC-GUI-master//TCC-GUI")
    ```
    If you are using RStudio, there will be a `Run App` button in the souce code file panel when you open file `ui.R`, `server.R` or `global.R`. Click the button and TCC-GUI will be launched.

If the above method still does not work, please try the old version installation method below or feel free to contact us.

<details>
<summary><b>Old Installation Method</b></summary>  
    
### Pre-installation

Make sure that you have already installed those packages in your environment.   

`shiny`, `shinydashboard`, `shinyWidgets`, `plotly`, `dplyr`, `TCC`, `DT`, `heatmaply`,  `rmarkdown`, `data.table`, `tidyr`, `RColorBrewer`, `utils`, `knitr`, `cluster`, `shinycssloaders`, `shinyBS`, `renv`, `MASS`.    

If any package is missing, Please run the following command in your [`RStudio`](https://www.rstudio.com/) and it will install all packages automatically.  

```R
# Check "BiocManager"
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# Package list
libs <- c("shiny", "shinydashboard", "shinyWidgets", "plotly", "dplyr", "DT", "heatmaply", "tidyr","utils","rmarkdown","data.table","RColorBrewer", "knitr", "cluster", "shinycssloaders", "shinyBS", "renv", "MASS", "TCC")

# Install packages if missing
for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
     BiocManager::install(i, suppressUpdates=TRUE)
  }
}
```

### Start the App

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

---

If you have any question about `TCC-GUI`, simply [create a issue for help (prefer)](https://github.com/swsoyee/TCC-GUI/issues/new/choose) or send E-mail to us. We will answer your question as soon as possible.

## 📕 Publication

If you have use `TCC-GUI` in your work, please cite the original paper and consider to give this repository a ⭐Star!  

> **TCC-GUI: a Shiny-based application for differential expression analysis of RNA-Seq count data**  
Wei Su, Jianqiang Sun, Kentaro Shimizu and Koji Kadota  
*BMC Research Notes* 2019 **12**:133  
https://doi.org/10.1186/s13104-019-4179-2 | © The Author(s) 2019  
**Received:** 14 January 2019 | **Accepted:** 11 March 2019 | **Published:** 13 March 2019

## 📚 References

1. Sun J, Nishiyama T, Shimizu K, et al. **TCC**: an R package for comparing tag count data with robust normalization strategies. *BMC bioinformatics*, 2013, 14(1): 219.  
2. Robinson M D, McCarthy D J, Smyth G K. **edgeR**: a Bioconductor package for differential expression analysis of digital gene expression data. *Bioinformatics*, 2010, 26(1): 139-140.  
3. Love M I, Huber W, Anders S. Moderated estimation of fold change and dispersion for RNA-seq data with **DESeq2**. *Genome biology*, 2014, 15(12): 550.  
4. Hardcastle T J, Kelly K A. **baySeq** : empirical Bayesian methods for identifying differential expression in sequence count data. *BMC bioinformatics*, 2010, 11(1): 422.  

## Code of Conduct

Please note that the TCC-GUI project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.  
