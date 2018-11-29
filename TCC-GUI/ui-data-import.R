# ui-data-import.R

fluidPage(fluidRow(column(
  3,
  tabBox(
    title = "Source",
    id = "datasource",
    width = NULL,
    tabPanel(
      tagList(icon("folder-open"), "Sample"),
      selectInput(
        "SampleDatabase",
        "Select Sample Data",
        choices = c(
          "hypodata (example dataset)" = "sample_data/data_hypodata_3vs3.txt" #,
          # "katz.mouse" = "sample_data/katzmouse_count_table.txt",
          # "cheung" = "sample_data/cheung_count_table.txt"
        )
      ),
      helpText("Quick start with sample data."),
      do.call(actionBttn, c(
        list(
          inputId = "CountDataSample",
          label = "1. Import Count Data",
          icon = icon("play")
        ),
        actionBttnParams
      ))
    ),
    tabPanel(
      tagList(icon("cloud-upload"), "Upload"),
      fileInput(
        "uploadCountData",
        "Upload Count Data",
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        buttonLabel = "Upload...",
        placeholder = "No file has been uploaded."
      ),
      helpText("Text file in tab-delimited format, and the first column is genes' name.")
    )
  )
  ,
  box(
    title = tagList(icon("sitemap"), "Group Assignment"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    textAreaInput(
      "groupSelectViaText",
      "Input your group info",
      rows = 6,
      placeholder = paste(
        "G1_rep1,Group1",
        "G1_rep2,Group1",
        "G1_rep3,Group1",
        "G2_rep1,Group2",
        "G2_rep2,Group2",
        "G2_rep3,Group2",
        sep = '\n'
      )
    ),
    do.call(actionBttn, c(
      list(
        inputId = "confirmedGroupList",
        label = "2. Assign Group Label",
        icon = icon("play")),
        actionBttnParams
      )
    ),
    footer = helpText("TCC-GUI expect first label should be Group1 (G1) and the next label be Group2 (G2), and so on.")
  ),
  box(
    title = tagList(icon("info-circle"), "Summary of Data"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    uiOutput("rowOfCountData"),
    uiOutput("groupCount"),
    uiOutput("zeroValue"),
    uiOutput("silhouette")
  )
),
column(
  9,
  box(
    title = tagList(icon("table"), "Read Count Table"),
    solidHeader = TRUE,
    status = "info",
    width = NULL,
    uiOutput("emptyTable")
  ),
  box(
    title = tagList(icon("bar-chart"), "Count Distribution (Box Plot)"),
    solidHeader = TRUE,
    status = "info",
    fluidRow(column(
      1,
      dropdownButton(
        tags$h3("Plot options"),
        textInput(
          inputId = "sampleDistributionTitle",
          label = "Title",
          value = "",
          placeholder = "Original Raw Count"
        ),
        numericInput(
          inputId = "sampleDistributionLegendY",
          label = "Legend Y",
          value = 1
        ),
        textInput(
          inputId = "sampleDistributionXlab",
          label = "X label",
          value = "Sample",
          placeholder = "Sample"
        ),
        textInput(
          inputId = "sampleDistributionYlab",
          label = "Y label",
          value = "log<sub>2</sub>(Count + 1)",
          placeholder = "log<sub>2</sub>(Count + 1)"
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
    title = tagList(icon("area-chart"), "Count Distribution (Density Plot)"),
    solidHeader = TRUE,
    status = "info",
    fluidRow(column(
      1,
      dropdownButton(
        tags$h3("Plot options"),
        textInput(
          inputId = "sampleDistributionDenstityTitle",
          label = "Title",
          value = "",
          placeholder = "Original Raw Count"
        ),
        numericInput(
          inputId = "sampleDistributionDensityLegendY",
          label = "Legend Y",
          value = 1.1
        ),
        textInput(
          inputId = "sampleDistributionDensityXlab",
          label = "X label",
          value = "log<sub>2</sub>(Count + 1)",
          placeholder = "log<sub>2</sub>(Count + 1)"
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
