library(shiny) 

shinyUI(fluidPage(
  useShinyalert(),
  navbarPage("TCC GUI Version",
             theme = shinytheme("cosmo"),
             tabsetPanel(id = "tabs",
               tabPanel("Welcome!",
                        source(file = "ui-homepage.R", local = TRUE, encoding = "UTF-8")$value
                        ),
               tabPanel("Computation",
                        source(file = "ui-data-import.R", local = TRUE, encoding = "UTF-8")$value
                        )
             ),
             # Footer
             tags$hr(),
             tags$p("Copyright (c)2018 Bioinformation Engineering Lab", align="center"), 
             tags$p("Department of Biotechnology, Graduate School of Agricultural and Life Science, The University of Tokyo All Rights Reserved.", align="center"),
             tags$p("Code available on Github:", 
                    a("TCC-GUI", href="https://github.com/swsoyee/TCC-GUI"), align="center")
  )#navbarPage
))
