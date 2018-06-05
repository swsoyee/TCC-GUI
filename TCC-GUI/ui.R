library(shiny)

shinyUI(fluidPage(
  useShinyalert(),
  navbarPage(
    "TCC GUI Version",
    # position = "fixed-top",
    theme = shinytheme("cosmo"),
    # TCC Narvbar
    tabPanel("TCC",
             tabsetPanel(
               id = "tabs",
               tabPanel("Guidance",
                        source(
                          file = "ui-homepage.R",
                          local = TRUE,
                          encoding = "UTF-8"
                        )$value),
               tabPanel("Computation",
                        source(
                          file = "ui-data-import.R",
                          local = TRUE,
                          encoding = "UTF-8"
                        )$value)
             )),
    # # NOISeq Narvbar
    # tabPanel("NOISeq",
    #          tabsetPanel(
    #            id = "tabs-noiseq",
    #            tabPanel("Guidance"),
    #            tabPanel("Computation")
    #          )),
    # # edgeR Narvbar
    # tabPanel("edgeR",
    #          tabsetPanel(
    #            id = "tabs-edger",
    #            tabPanel("Guidance"),
    #            tabPanel("Computation")
    #          )),
    # # DESeq2 Narvbar
    # tabPanel("DESeq2",
    #          tabsetPanel(
    #            id = "tabs-deseq2",
    #            tabPanel("Guidance"),
    #            tabPanel("Computation")
    #          )),
    # # DESeq2 Narvbar
    # tabPanel("Comparison",
    #          tabsetPanel(
    #            id = "tabs-compare"
    #          )),
    # Footer
    tags$hr(),
    tags$br(),
    tags$br(),
    tags$br(),
    absolutePanel(
      bottom = 0,
      left = 0,
      right = 0,
      fixed = TRUE,
      div(style = "padding: 10px; border-bottom: 1px solid #CCC; background: #222222; color: #FFFFFF; opacity: 0.9",
          HTML(
            markdownToHTML(
              fragment.only = TRUE,
              text = c(
                "Copyright (c)2018 Bioinformation Engineering Lab  
                Department of Biotechnology, Graduate School of Agricultural and Life Science, The University of Tokyo All Rights Reserved.  
                Code available on Github: [TCC-GUI](https://github.com/swsoyee/TCC-GUI)
                "
              )
              )
              ))
              )
            )#navbarPage
          ))
