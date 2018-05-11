library(shiny) 

shinyUI(fluidPage(
  
  navbarPage("TCC GUI Version",
             theme = shinytheme("cosmo"),
             tabsetPanel(
               tabPanel("Computation",
                        source(file = "ui-data-import.R", local = TRUE, encoding = "UTF-8")$value
                        ),
               tabPanel("MA Plot",
                        source(file = "ui-ma-plot.R", local = TRUE, encoding = "UTF-8")$value
                        ),
               tabPanel("Volcano Plot",
                        source(file = "ui-volcano-plot.R", local = TRUE, encoding = "UTF-8")$value
               )
             )
  )#navbarPage
))
