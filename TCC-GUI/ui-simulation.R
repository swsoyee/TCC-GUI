# ui-simulation.R

fluidPage(column(
  3,
  box(
    title = tagList(icon("gears"), "Data Simulation Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tipify(
      numericInput(
        inputId = "simulationSeed",
        label = "Set Random Seed",
        value = -1,
        min = -1,
        step = 1
      ),
      title = "Set -1 to switch off random seed"
    ),
    numericInput(
      inputId = "simulationGeneNum",
      label = HTML("Number of Genes (N<sub>gene</sub>)"),
      value = 10000
    ),
    sliderInput(
      inputId = "simulationPDEG",
      label = HTML("Proportion of DEGs (P<sub>DEG</sub>)"),
      min = 0,
      max = 1,
      value = 0.2
    ),
    textOutput("expectedDEGsText"),
    sliderInput(
      inputId = "simulationGroupNum",
      label = HTML("Number of Groups (N<sub>group</sub>)"),
      min = 2,
      value = 2,
      max = 10
    ),
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
    title = tagList(icon("circle-info"), "Summary"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    htmlOutput("simuParams")
  )
),
column(
  9,
  box(
    title = tagList(icon("wrench"), "Group Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("simulationGroup")
  ),
  box(
    title = tagList(icon("table"), "Simulation Data"),
    width = NULL,
    solidHeader = TRUE,
    status = "info",
    uiOutput("simuDataTableAndDownload")
  )
))