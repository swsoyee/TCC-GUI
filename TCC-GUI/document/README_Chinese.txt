# TCC-GUI

**TCC: Differential expression analysis for tag count data with robust normalization strategies**

![TCC LOGO](../www/tccLogo.png)

本软件包提供一系列的使用了稳健的标准化策略进行RNA-seq差异表达分析（俗称差异表达基因DEGES）的计算方法。该方法的基本标准化策略为在进行各个样本的表达情况的标准化之前首先把潜在的DEGES进行移除后再进行剩余基因的标准化，从而对为DEGES的可能性从高到低进行排序进行DEGES筛选。该方法通过进行多步标准化进行实现（称为差异表达基因的移除策略）。TCC通过调用其他分析包进行组合运用，能够对各类比对提供一个稳健的标准化方法。

作者: 孙建强，西山智明，清水谦多郎，门田幸二

GUI 版本开发者: 苏玮

### <a name="Datainput"></a> 1. 数据输入
---
- 点击画面左上方的 **"Computation"** 标签；
- 点击 **"Load Sample Data"** 按键加载测试用数据集，或者点击 **"Upload..."** 上传分析用的基因表达Count data的csv文本文件。
  ![Raw Count Table](../www/RawCountTable.png)

### <a name="Computation"></a> 2. 计算

---
- 在分组选择中选择分组数 **"Group Count"** 后按 **"Confirmed"** 按键；

- 在对应的分组中选择相应的列名后按 **"Confirmed"** 按键后，在画面右侧TCC相关参数面板 **"TCC Parameters"** 将会弹出。

  ![Group Selection](../www/GroupSelection.png)

- 根据需要更改相关参数后按 **"Run TCC"** 按键将会进行计算，请耐心等待。

  ![TCC Parameters](../www/TCC.png)

- 计算完成后在画面中部下方出现结果列表 **"Result Table"**，在画面上方出现一系列的后续分析标签。

  ![Result Table](../www/ResultTable.png)

### <a name="MAplot"></a> 3.1 MA图

---
- 计算完成后切换到 **"MA Plot"** 标签后点击 **"Generate MA-Plot"** 按键（相关参数可根据需要更改）；

- 将光标置于图中点处将会在画面右侧弹出该基因的表达情况，和提供相关信息。

  ![MA Plot](../www/maplot1-md.png)

- 如果需要在图上对具体基因进行标记，则可以在画面中下部的表中点选定相应基因后再次点击**"Generate MA-Plot" **按键，将会刷新绘图并在图上进行标记。

  ![MA Plot Marker](../www/maplot2-md.png)

- (后续功能可能会持续添加)

### <a name="Volcanoplot"></a> 3.2 火山图

---
- 和**MA图**使用方法雷同。
 ![volcanoplot](../www/volcanoplot.png)
- (后续功能可能会持续添加)

### <a name="PCAanalysis"></a> 3.3 PCA分析

---
- 根据需要更改相关参数后按 **"Run"** 按键将会进行计算和绘制。

  ![PCA Parameters](../www/PCAParameters.png)

  ![pca](../www/pca.png)

- (后续功能可能会持续添加)

### <a name="Heatmap"></a> 3.4 Heatmap

---
- 根据需要更改相关参数后按 **"Run"** 按键将会进行绘制，请耐心等待。

  ![Heatmap Parameters](../www/HeatmapParameters.png)

  ![heatmap](../www/heatmap.png)

- (后续功能可能会持续添加)

### <a name="Expression"></a> 3.5 表达图

---
- 在左侧的面板中选择想要绘制的基因后将会自动绘制。 目前支持**条形图**和**箱线图**。
   ![expression](../www/expressionPlot.png)

- (后续功能可能会持续添加)

   
### <a name="Morehelps"></a> 4. 更多帮助

---

- [Install Original TCC package from Bioconductor](http://www.bioconductor.org/packages/release/bioc/html/TCC.html)
