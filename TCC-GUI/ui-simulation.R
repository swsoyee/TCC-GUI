# ui-simulation.R

fluidPage(column(
  3,
  box(
    title = tagList(icon("cogs"), "Simulation Data Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    numericInput(inputId = "simulationGeneNum", label = "Number of Genes", value = 10000),
    sliderInput(inputId = "simulationPDEG", label = "Proportion of DEGs (total)", min = 0, max = 1, value = 0.2),
    textOutput("expectedDEGsText"),
    sliderInput(inputId = "simulationGroupNum", label = "Number of Groups", min = 2, value = 2, max = 10),
    do.call(actionBttn, c(
      list(
        inputId = "simulationRun",
        label = "Generate Simulation Data",
        icon = icon("play")
      ),
      actionBttnParams
    )),
    footer = "Only support single-factor experimental design now."
  ),
  box(
    title = tagList(icon("info-circle"), "Summary"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    verbatimTextOutput("simuParams")
  ),
  uiOutput("simulationGroupInfo")
),
column(
  9,
  box(title = tagList(icon("wrench"), "Group Parameters"),
      width = NULL,
      solidHeader = TRUE,
      status = "primary",
      uiOutput("simulationGroup")), 
  box(
    title = tagList(icon("table"), "Simulation Data"),
    width = NULL,
    solidHeader = TRUE,
    status = "info",
    uiOutput("simuDataTableAndDownload")
  )
))