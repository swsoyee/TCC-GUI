---
typora-root-url: ..\www
---

# TCC-GUI

**TCC: Differential expression analysis for tag count data with robust normalization strategies**

![TCC LOGO](/tccLogo.png)

This package provides a series of functions for performing differential expression analysis from RNA-seq count data using robust normalization strategy (called DEGES). The basic idea of DEGES is that potential differentially expressed genes or transcripts (DEGs) among compared samples should be removed before data normalization to obtain a well-ranked gene list where true DEGs are top-ranked and non-DEGs are bottom ranked. This can be done by performing a multi-step normalization strategy (called DEGES for DEG elimination strategy). A major characteristic of TCC is to provide the robust normalization methods for several kinds of count data (two-group with or without replicates, multi-group/multi-factor, and so on) by virtue of the use of combinations of functions in depended packages.

作者: Jianqiang Sun, Tomoaki Nishiyama, Kentaro Shimizu, 和 Koji Kadota

GUI 版本开发者: Wei Su

### 1. 数据输入
---
- 点击画面左上方的 **"Computation"** 标签；
- 点击 **"Load Sample Data"** 按键加载测试用数据集，或者点击 **"Upload..."** 上传分析用的基因表达Count data的csv文本文件。

### 2. 计算

---
- Select **"Group Count"** and click **"Confirmed"** button;
- Select columns name of your dataset for grouping and click **"Confirmed"**, then the **"TCC Parameters"** Panel will show up;
- Change the parameters for computation (if you wish), and click the **"Run TCC"**.
- After computation, **"Result Table"**, and series tab for other analysis will show up.

### 3.1 MA图

---
- After computation, switch to **"MA Plot"** tab, and click **"Generate MA-Plot"** button;

- Hover cursor on the point, and the additional information will be provided (Gene expression plot).

  ![MA Plot](/maplot1-md.png)

- If you want to mark some gene on the plot, please click the specific rows of gene, and click **"Generate MA-Plot" **button again to refresh the plot.

  ![MA Plot Marker](/maplot2-md.png)

- （后期功能完善进行中）

### 3.2 火山图

---
- 使用和操作方法基本和**MA图**部分雷同。
 ![volcanoplot](/volcanoplot.png)
- （后期功能完善进行中）

### 3.3 PCA分析

---
- 调整PCA参数后点击**"Run"**按键即可获得处理结果。

  ![pca](/pca.png)

- （后期功能完善进行中）

### 3.4 差异表达基因热图

---
- Change the parameters for heatmap (if you wish), and click the **"Run"** button.![heatmap](/heatmap.png)
- （后期功能完善进行中）

### 4. 更多帮助

- [从Bioconductor安装原始的TCC包进行分析](http://www.bioconductor.org/packages/release/bioc/html/TCC.html)