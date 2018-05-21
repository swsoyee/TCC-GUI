library(shinythemes)
library(shinyalert)
library(plotly)
library(colourpicker)
library(dplyr)
library(TCC)
library(DT)

sample_data_url <- "sample_data/data_hypodata_3vs3.txt"

sum_gene <- function(x, df){
  sum(df$q.value<=x)
}

convert2cl <- function(x, df) {
  grep(x, colnames(df))
}