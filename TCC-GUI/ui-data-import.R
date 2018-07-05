# ui-data-import.R

fluidPage(fluidRow(
  column(
    3,
    tags$hr(),
    wellPanel(
      tags$h4("Count Data Input"),
      tags$hr(),
      selectInput(
        "SampleDatabase",
        "Select Sample Data",
        choices = c("hypodata" = "sample_data/data_hypodata_3vs3.txt")
      ),
      actionButton("CountDataSample", "Load Sample Data"),
      tags$hr(),
      fileInput(
        "uploadCountData",
        "Upload Count Data",
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"),
        buttonLabel = "Upload...",
        placeholder = "No file has been uploaded."
      )
    ),
    #wellPanel
    wellPanel(
      tags$h4("Group Selection"),
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
    )#wellPanel
    
  ),
  #column
  column(
    6,
    tags$hr(),
    tags$h3("Raw Count Table"),
    uiOutput("emptyTable"),
    uiOutput("mainResultTable")
  ),
  #column
  column(
    3,
    tags$hr(),
    wellPanel(tags$h4("TCC Parameters"),
              tags$hr(),
              uiOutput("TCC")),
    uiOutput("summaryTCC")
  )
))
