# What's TCC?  
---
![TCC LOGO](../www/tccLogo.png)

**TCC (Differential expression analysis for tag count data with robust normalization strategies)** is a package provides a series of functions for performing differential expression analysis from RNA-seq count data using robust normalization strategy (called **DEGES**).   

The basic idea of **DEGES** is that potential differentially expressed genes or transcripts (**DEGs**) among compared samples should be removed before data normalization to obtain a well-ranked gene list where true **DEGs** are top-ranked and **non-DEGs** are bottom ranked.   

This can be done by performing a multi-step normalization strategy (called **DEGES** for **DEG elimination strategy**).  

A major characteristic of **TCC** is to provide the robust normalization methods for several kinds of count data (**two-group with or without replicates**, **multi-group/multi-factor**, and so on) by virtue of the use of combinations of functions in depended packages.

Author: Jianqiang Sun, Tomoaki Nishiyama, Kentaro Shimizu, and Koji Kadota

GUI Version Developer: Wei Su  



# TCC-GUI Version: Graphical User Interface for Tag Count Comparison (TCC) package
---
In this **GUI version of TCC**, all parameter settings are available just like you are using the original one. Besides, the **GUI version** also provide lots of plotting function where the original package is unsupported now. 

#### Function

- Sample Distribution Plot
- TCC Computation
- MA Plot
- Volcano Plot
- PCA Analysis (clustering included)
- Heatmap Plot (clustering included)
- Expression Level Plot
- Output result in table, figure, code or report (.md, .pdf)

Please check other tab in Guidance for details.  

If you have any question about the application, comment and advise, please go to [github](https://github.com/swsoyee/TCC-GUI/issues) and open a issue (you can write in English, Chinese or Japanese as you like).

