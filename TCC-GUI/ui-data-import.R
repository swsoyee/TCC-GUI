# ui-data-import.R

fluidRow(
  column(3, 
         wellPanel(
           tags$h4("Count Data Input"),
           tags$hr(),
           actionButton("CountDataSample", "Load Sample Data"),
           tags$hr(),
           fileInput("uploadCountData", "Upload Count Data", 
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"),
                     buttonLabel = "Upload...",
                     placeholder = "No file has been uploaded.")
         ),#wellPanel
         wellPanel(
           tags$h4("Group Selection"),
           uiOutput("groupSlide"),
           uiOutput("groupSelect"),
           uiOutput("confirmedGroupList")
         )#wellPanel
         
  ),#column
  column(6,
         tags$hr(),
         tags$h3("Raw Count Table"),
         DT::dataTableOutput('table'),
         uiOutput("mainResultTable")
  ),#column
  column(3,
         tags$hr(),
         wellPanel(
           tags$h4("TCC Parameters"),
           tags$hr(),
           uiOutput("TCC")
         )
  )
)