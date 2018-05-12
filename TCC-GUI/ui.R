library(shiny) 

shinyUI(fluidPage(
  
  navbarPage("TCC GUI Version",
             theme = shinytheme("cosmo"),
             tabsetPanel(id = "tabs",
               tabPanel("Computation",
                        source(file = "ui-data-import.R", local = TRUE, encoding = "UTF-8")$value
                        )
             )
  )#navbarPage
))
