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
        "G1_rep1,control",
        "G1_rep2,control",
        "G1_rep3,control",
        "G2_rep1,sample",
        "G2_rep2,sample",
        "G2_rep3,sample",
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
    status = "info",
    width = NULL,
    uiOutput("emptyTable")
  )
)),
fluidRow(column(
  3,
  box(
    title = "Summary of Data",
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    uiOutput("rowOfCountData"),
    uiOutput("ColumnOfCountData"),
    uiOutput("groupCount"),
    uiOutput("zeroValue")
  )
),
column(
  9,
  box(
    title = "Sample Distribution",
    solidHeader = TRUE,
    status = "info",
    fluidRow(column(
      1,
      dropdownButton(
        tags$h3("Plot options"),
        textInput(
          inputId = "sampleDistributionTitle",
          label = "Title",
          value = "Row Count Sample Distribution",
          placeholder = "Row Count Sample Distribution"
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
    title = "Row Count Distribution",
    solidHeader = TRUE,
    status = "info",
    fluidRow(column(
      1,
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
