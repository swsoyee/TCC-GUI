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
          "hypodata" = "sample_data/data_hypodata_3vs3.txt",
          "katz.mouse" = "sample_data/katzmouse_count_table.txt",
          "cheung" = "sample_data/cheung_count_table.txt"
        )
      ),
      actionBttn(
        "CountDataSample",
        "Load Sample Data",
        icon = icon("plus-square"),
        size = "sm",
        color = "primary",
        style = "fill"
      )
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
      )
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
        "G1_rep1,1",
        "G1_rep2,1",
        "G1_rep3,1",
        "G2_rep1,2",
        "G2_rep2,2",
        "G2_rep3,2",
        sep = '\n'
      )
    ),
    actionBttn(
      inputId = "confirmedGroupList",
      label = "Confirmed",
      icon = icon("check-square"),
      size = "sm",
      color = "primary",
      style = "fill"
    )
  )
),
column(
  9,
  box(
    title = "Read Count Table",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("emptyTable")
  )
)),
fluidRow(column(
  3,
  box(
    title = "Summary of Data",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("rowOfCountData"),
    uiOutput("ColumnOfCountData"),
    uiOutput("groupCount"),
    uiOutput("sampleInGroup")
  )
),
column(
  9,
  box(
    title = "Sample Distribution",
    solidHeader = TRUE,
    status = "primary",
    withBarsUI(plotlyOutput("sampleDistribution"))
  ),
  box(
    title = "Row Count Distribution",
    solidHeader = TRUE,
    status = "primary",
    dropdownButton(
      tags$h3("Plot options"),
      textInput(
        inputId = "sampleDistributionDensityTitle",
        label = "Title",
        value = "Row Count Distribution",
        placeholder = "Row Count Distribution"
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
    ),
    dropdownButton(
      tags$h3("R code"),
      tags$p("Coming soon"),
      status = "info",
      icon = icon("code"),
      size = "sm",
      tooltip = tooltipOptions(title = "Show R code")
    ),
    withBarsUI(plotlyOutput("sampleDistributionDensity"))
  )
)))
