library(shinydashboard)
library(plotly)
library(dplyr)
library(TCC)
library(DT)
library(heatmaply)
library(data.table)
library(RColorBrewer)
library(markdown)
library(plotlyBars)
library(utils)
library(shinyWidgets)

sample_data_url <- "sample_data/data_hypodata_3vs3.txt"

convert2cl <- function(x, df) {
  grep(x, colnames(df))
}

# ====================================
# This function input a result table of TCC, and make a summary data.table
# of gene count under different FDR Cutoff.
# Position: Computation tab, MA Plot tab, Volcano Plot tab.
# ====================================

make_summary_for_tcc_result <- function(df){
  # Make some function for create new column
  sum_gene <- function(x, df){
    sum(df$q.value<=x)
  }
  
  # Set different cut-off
  span <- c(0, 0.01, seq(0.05, 1, 0.05))
  # Calculate gene count under specific cut-off
  deg_in_cutoff <- sapply(span, sum_gene, df)
  # Calculate total gene count
  total_gene <- nrow(df)
  
  # Create table
  df <- data.frame(
    "Cutoff" = sprintf('%#.2f', span),
    "Under_Count" = deg_in_cutoff,
    "Percentage" = paste(round(deg_in_cutoff / total_gene, 4) * 100, "%")
  )
  df <-
    tbl_df(df) %>% mutate(Between_Count = Under_Count - lag(Under_Count)) %>%
    mutate(Count = paste0(Under_Count, "(+", Between_Count, ")"))
  
  return(df)
}