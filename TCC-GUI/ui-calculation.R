# ui-calculation.R

fluidPage(useSweetAlert(), fluidRow(column(
  3,
  box(
    title = tagList(icon("gears"), "TCC Computation Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(
      selectInput(
        "normMethod",
        "Normalization Method",
        c("TMM" = "tmm",
          "DESeq2" = "deseq2")
      ),
      selectInput(
        "testMethod",
        "DEG Identification Method",
        c(
          "edgeR" = "edger",
          "DESeq2" = "deseq2",
          "baySeq" = "bayseq",
          "SAMSeq" = "samseq",
          "Voom" = "voom"#,
          # "WAD" = "wad"
        )
      ),
      popify(
        sliderTextInput(
          inputId = "filterLowCount",
          label = "Filtering Threshold for Low Count Genes",
          choices = c("Do not filter", c(0:30))
        ),
        title = "Reference",
        content = '<p>Filter genes with a total read count smaller than thresholds.</p><p>Sultan, Marc, et al. <a href="http://science.sciencemag.org/content/321/5891/956">"A global view of gene activity and alternative splicing by deep sequencing of the human transcriptome."</a> <i>Science</i> 321.5891 (2008): 956-960.</p>',
        placement = "top"
      ), 
      textOutput("lowCountFilterText"),
      sliderInput(
        "iteration",
        "Number of Iteration",
        min = 0,
        max = 30,
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
          label = "Run TCC Computation",
          icon = icon("play")
        ),
        actionBttnParams
      ))
    )
  ),
  box(
    title = tagList(icon("code"), "TCC Computation Code"),
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
  box(
    title = tagList(icon("table"), "Summary of TCC Normalization"),
    width = NULL,
    solidHeader = TRUE,
    status = "info",
    uiOutput("tccSummationUI")
  )
)))