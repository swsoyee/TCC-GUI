# ui-calculation.R

fluidPage(fluidRow(column(
  4,
  box(
    title = "TCC Parameters",
    width = NULL,
    tagList(
      selectInput(
        "normMethod",
        "Normalization method:",
        c("TMM" = "tmm",
          "DESeq" = "deseq")
      ),
      selectInput(
        "testMethod",
        "DEGs identify method:",
        c(
          "edgeR" = "edger",
          "DESeq" = "deseq",
          "DESeq2" = "deseq2",
          "baySeq" = "bayseq",
          "SAMSeq" = "samseq",
          "Voom" = "voom",
          "WAD" = "wad"
        )
      ),
      numericInput(
        "filterLowCount",
        "Filter low count genes threshold:",
        value = 0,
        min = 0
      ),
      sliderInput(
        "iteration",
        "Interation:",
        min = 1,
        max = 50,
        value = 3
      ),
      sliderInput(
        "fdr",
        "FDR:",
        min = 0,
        max = 1,
        value = 0.1,
        step = 0.05
      ),
      sliderInput(
        "floorpdeg",
        "Elimination of Potential DEGs:",
        min = 0,
        max = 1,
        value = 0.05,
        step = 0.05
      ),
      fluidRow(column(6, actionButton("TCC", "Run TCC")),
               column(6, uiOutput("runTCCCode")))
    )
  )
),
column(
  8,
  box(
    title = "Result Table",
    width = NULL,
    uiOutput("mainResultTable")
    # fluidRow(
    #   downloadButton("downLoadResultTable", "Download TCC Result"),
    #   downloadButton("downLoadNormalized", "Download Normalized Data")
    # ),
    # fluidRow(DT::dataTableOutput('resultTable'))
  )
)))