# ui-data-import.R

fluidPage(fluidRow(column(
  3,
  tabBox(
    title = "Data Source",
    id = "datasource",
    width = NULL,
    tabPanel(
      "Sample",
      selectInput(
        "SampleDatabase",
        "Select Sample Data",
        choices = c(
          "hypodata" = "sample_data/data_hypodata_3vs3.txt" #,
          # "katz.mouse" = "sample_data/katzmouse_count_table.txt",
          # "cheung" = "sample_data/cheung_count_table.txt"
        )
      ),
      tags$p("Quick start with sample data."),
      do.call(actionBttn, c(
        list(
          inputId = "CountDataSample",
          label = "Import Data",
          icon = icon("plus-square")
        ),
        actionBttnParams
      ))
    ),
    tabPanel(
      "Upload",
      fileInput(
        "uploadCountData",
        "Upload Count Data",
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        buttonLabel = "Upload...",
        placeholder = "No file has been uploaded."
      ),
      tags$p("Text file in tab-delimited format, and the first column is genes' name.")
    )
  )
  ,
  box(
    title = "Group Selection",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    textAreaInput(
      "groupSelectViaText",
      "Input your group info:",
      rows = 6,
      placeholder = paste(
        "G1_rep1,control",
        "G1_rep2,control",
        "G1_rep3,control",
        "G2_rep1,treatment",
        "G2_rep2,treatment",
        "G2_rep3,treatment",
        sep = '\n'
      )
    ),
    do.call(actionBttn, c(
      list(
        inputId = "confirmedGroupList",
        label = "Confirm",
        icon = icon("check-square")),
        actionBttnParams
      )
    )
  ),
  box(
    title = "Summary of Data",
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    uiOutput("rowOfCountData"),
    uiOutput("groupCount"),
    uiOutput("zeroValue")
  )
),
# column(
#   9,
#   box(
#     title = "Read Count Table",
#     solidHeader = TRUE,
#     status = "info",
#     width = NULL,
#     uiOutput("emptyTable")
#   )
# )),
# fluidRow(column(
#   3,
#   # box(
#   #   title = "Summary of Data",
#   #   solidHeader = TRUE,
#   #   status = "info",
#   #   width = NULL,
#   #   uiOutput("rowOfCountData"),
#   #   uiOutput("groupCount"),
#   #   uiOutput("zeroValue")
#   # )
# ),
column(
  9,
  box(
    title = "Read Count Table",
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    uiOutput("emptyTable")
  ),
  box(
    title = "Sample Distribution",
    solidHeader = TRUE,
    status = "info",
    fluidRow(column(
      1,
      dropdownButton(
        tags$h3("Plot options"),
        numericInput(
          inputId = "sampleDistributionLegendY",
          label = "Legend Y",
          value = 1.2
        ),
        textInput(
          inputId = "sampleDistributionXlab",
          label = "X label",
          value = "Sample",
          placeholder = "log2(CPM)"
        ),
        textInput(
          inputId = "sampleDistributionYlab",
          label = "Y label",
          value = "log2(CPM)",
          placeholder = "Density"
        ),
        status = "primary",
        icon = icon("gear"),
        size = "sm",
        tooltip = tooltipOptions(title = "Plot options")
      )
    ),
    column(
      1,
      dropdownButton(
        tags$h3("R code"),
        tags$p("Coming soon"),
        status = "danger",
        icon = icon("code"),
        size = "sm",
        tooltip = tooltipOptions(title = "Show R code")
      )
    )),
    withBarsUI(plotlyOutput("sampleDistribution"))
  ),
  box(
    title = "Raw Count Distribution",
    solidHeader = TRUE,
    status = "info",
    fluidRow(column(
      1,
      dropdownButton(
        tags$h3("Plot options"),
        numericInput(
          inputId = "sampleDistributionDensityLegendY",
          label = "Legend Y",
          value = 1.2
        ),
        textInput(
          inputId = "sampleDistributionDensityXlab",
          label = "X label",
          value = "log2(CPM)",
          placeholder = "log2(CPM)"
        ),
        textInput(
          inputId = "sampleDistributionDensityYlab",
          label = "Y label",
          value = "Density",
          placeholder = "Density"
        ),
        status = "primary",
        icon = icon("gear"),
        size = "sm",
        tooltip = tooltipOptions(title = "Plot options")
      )
    ),
    column(
      1,
      dropdownButton(
        tags$h3("R code"),
        tags$p("Coming soon"),
        status = "danger",
        icon = icon("code"),
        size = "sm",
        tooltip = tooltipOptions(title = "Show R code")
      )
    )),
    withBarsUI(plotlyOutput("sampleDistributionDensity"))
  )
)))
