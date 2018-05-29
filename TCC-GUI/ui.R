library(shiny)

shinyUI(fluidPage(
  useShinyalert(),
  navbarPage(
    "TCC GUI Version",
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
    # NOISeq Narvbar
    tabPanel("NOISeq",
             tabsetPanel(
               id = "tabs-noiseq",
               tabPanel("Guidance"),
               tabPanel("Computation")
             )),
    # edgeR Narvbar
    tabPanel("edgeR",
             tabsetPanel(
               id = "tabs-edger",
               tabPanel("Guidance"),
               tabPanel("Computation")
             )),
    # DESeq2 Narvbar
    tabPanel("DESeq2",
             tabsetPanel(
               id = "tabs-deseq2",
               tabPanel("Guidance"),
               tabPanel("Computation")
             )),
    # DESeq2 Narvbar
    tabPanel("Comparison",
             tabsetPanel(
               id = "tabs-compare"
             )),
    # Footer
    tags$hr(),
    tags$p("Copyright (c)2018 Bioinformation Engineering Lab", align =
             "center"),
    tags$p(
      "Department of Biotechnology, Graduate School of Agricultural and Life Science, The University of Tokyo All Rights Reserved.",
      align = "center"
    ),
    tags$p(
      "Code available on Github:",
      a("TCC-GUI", href = "https://github.com/swsoyee/TCC-GUI"),
      align = "center"
    )
  )#navbarPage
))
