library(shiny)
tagList(
  dashboardPage(
    dashboardHeader(
      title = "TCC-GUI: Graphical User Interface for TCC package",
      titleWidth = 500,
      dropdownMenu(
        type = "messages",
        messageItem(
          from = "TCC Algorithm",
          message = "Go to BMC Bioinformatics",
          icon = icon("book"),
          # time = "Published 2013-07-09",
          href = "https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-219"
        ),
        messageItem(
          from = "Source Code",
          message = "Available on Github",
          # time = sprintf("Update ad %s", as.Date(max(
          #   file.info(list.files())$mtime
          # ))),
          href = "https://github.com/swsoyee/TCC-GUI"
        ),
        messageItem(
          from = "About Us",
          message = "Go to Lab Homepage",
          icon = icon("users"),
          href = "http://www.bi.a.u-tokyo.ac.jp/"
        ),
        icon = icon("circle-info"),
        headerText = "INFORMATIONS"
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "sider",
        menuItem("Documentation",
          tabName = "introduction",
          icon = icon("book")
        ),
        menuItem(
          "Data Simulation",
          tabName = "simulationData",
          icon = icon("shuffle"),
          badgeLabel = "Step 0",
          badgeColor = "yellow"
        ),
        menuItem(
          "Exploratory Analysis",
          tabName = "dateImport",
          icon = icon("flask"),
          badgeLabel = "Step 1",
          badgeColor = "orange"
        ),
        menuItemOutput("calculationTab"),
        menuItemOutput("maplotTab"),
        menuItemOutput("volcanoplotTab"),
        menuItemOutput("heatmapTab"),
        menuItemOutput("expressionTab"),
        menuItemOutput("reportTab")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "introduction",
          tabBox(
            title = "",
            width = NULL,
            tabPanel(
              title = "Welcome to TCC-GUI",
              icon = icon("info"),
              fluidRow(
                column(
                  includeMarkdown("document/English_Welcome.md"),
                  width = 10,
                  offset = 1
                )
              )
            ),
            tabPanel(
              title = "Data Simulation",
              icon = icon("shuffle"),
              fluidRow(column(
                includeMarkdown("document/English_Simulation_Data.md"),
                width = 10,
                offset = 1
              ))
            ),
            tabPanel(
              title = "Exploratory Analysis",
              icon = icon("flask"),
              fluidRow(column(
                includeMarkdown("document/English_Data_input.md"),
                width = 10,
                offset = 1
              ))
            ),
            tabPanel(
              title = "TCC Computation",
              icon = icon("calculator"),
              fluidRow(column(
                includeMarkdown("document/English_Computation.md"),
                width = 10,
                offset = 1
              ))
            ),
            tabPanel(
              title = "MA Plot",
              icon = icon("chart-line"),
              fluidRow(column(
                includeMarkdown("document/English_MA_plot.md"),
                width = 10,
                offset = 1
              ))
            ),
            tabPanel(
              title = "Volcano Plot",
              icon = icon("chart-area"),
              fluidRow(column(
                includeMarkdown("document/English_Volcano_plot.md"),
                width = 10,
                offset = 1
              ))
            ),
            tabPanel(
              title = "Heatmap",
              icon = icon("table-cells"),
              fluidRow(
                column(
                  includeMarkdown("document/English_Heatmap.md"),
                  width = 10,
                  offset = 1
                )
              )
            ),
            tabPanel(
              title = "Expression Level Plot",
              icon = icon("chart-column"),
              fluidRow(column(
                includeMarkdown("document/English_Expression.md"),
                width = 10,
                offset = 1
              ))
            ),
            tabPanel(
              title = "Analysis Report",
              icon = icon("file"),
              fluidRow(
                column(
                  includeMarkdown("document/English_More_help.md"),
                  width = 10,
                  offset = 1
                )
              )
            )
          )
        ),
        tabItem(tabName = "guidence", source(
          file = "ui-homepage.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value),
        tabItem(tabName = "simulationData", source(
          file = "ui-simulation.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value),
        tabItem(tabName = "dateImport", source(
          file = "ui-data-import.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value),
        tabItem(tabName = "calculationTab", source(
          file = "ui-calculation.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value),
        tabItem(tabName = "maplotTab", source(
          file = "ui-ma-plot.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value),
        tabItem(tabName = "volcanoplotTab", source(
          file = "ui-volcano-plot.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value),
        tabItem(tabName = "heatmapTab", source(
          file = "ui-heatmap.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value),
        tabItem(tabName = "expressionTab", source(
          file = "ui-expression-plot.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value),
        tabItem(tabName = "reportTab", source(
          file = "ui-report.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value)
      )
    )
  ),
  tags$footer(
    tags$p("Copyright © 2021"),
    tags$a("Bioinformation Engineering Lab, ", href = "http://www.bi.a.u-tokyo.ac.jp/"),
    tags$a(
      " Graduate School of Agricultural and Life Sciences / Faculty of Agriculture, ",
      href = "http://www.a.u-tokyo.ac.jp/english/index.html"
    ),
    tags$a(" The University of Tokyo ", href = "https://www.u-tokyo.ac.jp/en/index.html"),
    tags$p(sprintf("Version %s", gsub(
      "-", ".", as.Date(max(file.info(list.files())$mtime))
    ))),
    style = "
  bottom:0;
  width:100%;
  color: #B8C7CE;
  padding: 10px;
  background-color: #222D32;
  z-index: 1000;"
  )
)
