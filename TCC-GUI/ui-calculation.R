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
        tagList(
          "Filtering Threshold for Low Count Genes",
          helpText("(Set -1 for using all genes)")
        ),
        value = -1,
        min = -1
      ),
      textOutput("lowCountFilterText"),
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
    title = "",
    id = "sampleDistributionInTCC",
    width = NULL,
    tabPanel(title = tagList(icon("line-chart"), "Filtering Threshold"),
             fluidRow(
               column(
                 2,
                 sliderInput(
                   inputId = "lowCountSlide",
                   label = "Max Low Gene Count",
                   min = 3,
                   max = 100,
                   value = 50,
                   step = 1
                 )
               ),
               column(10, plotlyOutput("lowCountFilterByCutoff") %>% withSpinner())
             )),
    tabPanel(
      title = tagList(icon("table"), "Summary of TCC normalization"),
      fluidRow(column(12,
                      uiOutput("tccSummationUI")))
    ),
    tabPanel(
      title = tagList(icon("area-chart"), "Density Plot (Filtering)"),
      uiOutput("norDistributionDensityPanel")
    )
  )
)))