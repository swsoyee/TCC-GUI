# ui-data-import.R

fluidPage(fluidRow(column(
  4,
  tabBox(
    title = "Data Source",
    id = "datasource",
    width = NULL,
    tabPanel(
      "Sample Data",
      selectInput(
        "SampleDatabase",
        "Select Sample Data",
        choices = c(
          "hypodata" = "sample_data/data_hypodata_3vs3.txt",
          "katz.mouse" = "sample_data/katzmouse_count_table.txt",
          "cheung" = "sample_data/cheung_count_table.txt"
        )
      ),
      actionButton("CountDataSample", "Load Sample Data")
    ),
    tabPanel(
      "User's Data",
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
    actionButton("confirmedGroupList", "Confirmed")
  )
),
column(
  8,
  box(
    title = "Read Count Table",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("emptyTable")
  )
)),
fluidRow(column(
  4,
  box(
    title = "Summary of Data",
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    tableOutput("sampleSummary")
  )
),
column(
  8,
  box(title = "Sample Distribution",
      solidHeader = TRUE,
      status = "primary",
      withBarsUI(plotlyOutput(
        "sampleDistribution"
      ))),
  box(title = "Row Count Distribution",
      solidHeader = TRUE,
      status = "primary",
      withBarsUI(
        plotlyOutput("sampleDistributionDensity")
      ))
)))

# fluidPage(fluidRow(
#   column(
#     3,
#     tags$hr(),
#     wellPanel(
#       tags$h4("Count Data Input"),
#       tags$hr(),
#       selectInput(
#         "SampleDatabase",
#         "Select Sample Data",
#         choices = c("hypodata" = "sample_data/data_hypodata_3vs3.txt",
#                     "katz.mouse" = "sample_data/katzmouse_count_table.txt",
#                     "cheung" = "sample_data/cheung_count_table.txt")
#       ),
#       actionButton("CountDataSample", "Load Sample Data"),
#       tags$hr(),
#       fileInput(
#         "uploadCountData",
#         "Upload Count Data",
#         accept = c("text/csv",
#                    "text/comma-separated-values,text/plain",
#                    ".csv"),
#         buttonLabel = "Upload...",
#         placeholder = "No file has been uploaded."
#       )
#     ),
#     #wellPanel
#     wellPanel(
#       tags$h4("Group Selection"),
#       textAreaInput(
#         "groupSelectViaText",
#         "Input your group info:",
#         rows = 6,
#         placeholder = paste(
#           "G1_rep1,1",
#           "G1_rep2,1",
#           "G1_rep3,1",
#           "G2_rep1,2",
#           "G2_rep2,2",
#           "G2_rep3,2",
#           sep = '\n'
#         )
#       ),
#       actionButton("confirmedGroupList", "Confirmed")
#     )#wellPanel
#     
#   ),
#   #column
#   column(
#     6,
#     tags$hr(),
#     tags$h3("Raw Count Table"),
#     uiOutput("emptyTable"),
#     uiOutput("mainResultTable")
#   ),
#   #column
#   column(
#     3,
#     tags$hr(),
#     wellPanel(tags$h4("TCC Parameters"),
#               tags$hr(),
#               uiOutput("TCC")),
#     uiOutput("summaryTCC")
#   )
# ))
