# ui-calculation.R

fluidPage(useSweetAlert(), fluidRow(column(
  3,
  box(
    title = "TCC Parameters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
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
      actionBttn(
        "TCC",
        "Run TCC",
        icon = icon("play"),
        size = "sm",
        color = "primary",
        style = "fill"
      )
    )
  ),
  box(
    title = "TCC Calculation Code",
    status = "danger",
    solidHeader = TRUE,
    width = NULL,
    collapsible = TRUE,
    collapsed = TRUE,
    verbatimTextOutput("showTCCCode")
  )
),
column(
  9,
  box(
    title = "Result Table",
    width = NULL,
    solidHeader = TRUE,
    status = "info",
    uiOutput("mainResultTable")
  ),
  box(
    title = "Sample Distribution",
    width = NULL,
    solidHeader = TRUE,
    status = "info",
    column(6,
           withBarsUI(plotlyOutput(
             "sampleDistributionTCC"
           ))),
    column(6, uiOutput("sampleDistributionInTCC"))
  )
)))