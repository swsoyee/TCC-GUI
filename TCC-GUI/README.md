---
typora-root-url: www
---

# TCC-GUI

**TCC: Differential expression analysis for tag count data with robust normalization strategies**

![TCC LOGO](/tccLogo.png)

This package provides a series of functions for performing differential expression analysis from RNA-seq count data using robust normalization strategy (called DEGES). The basic idea of DEGES is that potential differentially expressed genes or transcripts (DEGs) among compared samples should be removed before data normalization to obtain a well-ranked gene list where true DEGs are top-ranked and non-DEGs are bottom ranked. This can be done by performing a multi-step normalization strategy (called DEGES for DEG elimination strategy). A major characteristic of TCC is to provide the robust normalization methods for several kinds of count data (two-group with or without replicates, multi-group/multi-factor, and so on) by virtue of the use of combinations of functions in depended packages.

Author: Jianqiang Sun, Tomoaki Nishiyama, Kentaro Shimizu, and Koji Kadota

GUI Version Developer: Wei Su

### 1. Data input
---
- Click **"Computation"** tab on the top;
- Click **"Load Sample Data"** button for test or Click **"Upload..."** button if you want to used your own count dataset.

### 2. Computation

---
- Select **"Group Count"** and click **"Confirmed"** button;
- Select columns name of your dataset for grouping and click **"Confirmed"**, then the **"TCC Parameters"** Panel will show up;
- Change the parameters for computation (if you wish), and click the **"Run TCC"**.
- After computation, **"Result Table"**, and series tab for other analysis will show up.

### 3.1 MA plot

---
- After computation, switch to **"MA Plot"** tab, and click **"Generate MA-Plot"** button;

- Hover cursor on the point, and the additional information will be provided (Gene expression plot).

  ![MA Plot](/maplot1-md.png)

- If you want to mark some gene on the plot, please click the specific rows of gene, and click **"Generate MA-Plot" **button again to refresh the plot.

  ![MA Plot Marker](/maplot2-md.png)

- (This is prerelease version. more functions need to be add in)

### 3.2 Volcano plot

---
- Same as part of **MA Plot**.
 ![volcanoplot](/volcanoplot.png)
- (This is prerelease version. more functions need to be add in)

### 3.3 PCA analysis

---
- Change the parameters for PCA (if you wish), and click the **"Run"** button.

  ![pca](/pca.png)

- (This is prerelease version. more functions need to be add in)

### 3.4 Heatmap

---
- Change the parameters for heatmap (if you wish), and click the **"Run"** button.![heatmap](/heatmap.png)
- (This is prerelease version. more functions need to be add in)

### 4. More helps

- [Install Original TCC package from Bioconductor](http://www.bioconductor.org/packages/release/bioc/html/TCC.html)