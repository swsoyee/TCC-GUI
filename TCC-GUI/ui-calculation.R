# ui-calculation.R

fluidPage(useSweetAlert(), fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "TCC Computation Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(
      selectInput(
        "normMethod",
        "Normalization Method",
        c("TMM" = "tmm",
          "DESeq" = "deseq")
      ),
      selectInput(
        "testMethod",
        "DEG Identification Method",
        c(
          "edgeR" = "edger",
          "DESeq" = "deseq",
          "DESeq2" = "deseq2",
          "baySeq" = "bayseq",
          "SAMSeq" = "samseq",
          "Voom" = "voom"#,
          # "WAD" = "wad"
        )
      ),
      numericInput(
        "filterLowCount",
        "Filtering Threshold for Low Count Genes",
        value = 0,
        min = 0
      ),
      sliderInput(
        "iteration",
        "Number of Iteration",
        min = 1,
        max = 50,
        value = 3
      ),
      sliderInput(
        "fdr",
        "FDR Cut-off",
        min = 0,
        max = 1,
        value = 0.1,
        step = 0.05
      ),
      sliderInput(
        "floorpdeg",
        "Elimination of Potential DEGs",
        min = 0,
        max = 1,
        value = 0.05,
        step = 0.05
      ),
      do.call(actionBttn, c(
        list(
          inputId = "TCC",
          label = "Run TCC Calculation",
          icon = icon("play")
        ),
        actionBttnParams
      ))
    )
  ),
  box(
    title = tagList(icon("code"), "TCC Calculation Code"),
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
    title = tagList(icon("table"), "Result Table"),
    width = NULL,
    solidHeader = TRUE,
    status = "info",
    uiOutput("mainResultTable")
  ),
  tabBox(
    title = "Count Distribution",
    id = "sampleDistributionInTCC",
    width = NULL,
    tabPanel(title = tagList(icon("bar-chart"), "Box Plot"),
             fluidRow(
               column(6,
                      withBarsUI(plotlyOutput(
                        "sampleDistributionTCC"
                      )))
               ,
               column(6,
                      withBarsUI(
                        plotlyOutput("NormalizedSampleDistribution")
                      ))
             )),
    tabPanel(title = tagList(icon("area-chart"), "Density Plot"),
             fluidRow(
               column(6,
                      withBarsUI(
                        plotlyOutput("sampleDistributionDensityTCC")
                      )),
               column(6,
                      withBarsUI(
                        plotlyOutput("NormalizedSampleDistributionDensity")
                      ))
             ))
  )
)))