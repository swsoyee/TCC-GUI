# ui-simulation.R

fluidPage(column(
  3,
  box(
    title = "Simulation Data Parameters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    numericInput(inputId = "simulationGeneNum", label = "Number of Genes", value = 10000),
    sliderInput(inputId = "simulationPDEG", label = "Proportion of DEGs", min = 0, max = 1, value = 0.2),
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
    title = "Summary",
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    verbatimTextOutput("simuParams")
  ),
  uiOutput("simulationGroupInfo")
),
column(
  9,
  box(title = "Group Parameters",
      width = NULL,
      solidHeader = TRUE,
      status = "primary",
      uiOutput("simulationGroup")), 
  box(
    title = "Simulation Data",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("simuDataTableAndDownload")
  )
))