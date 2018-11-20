library(shiny)

dashboardPage(
  dashboardHeader(title = "TCC-GUI: Graphical User Interface for TCC package", titleWidth = 500,
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "TCC Algorithm",
                                 message = "Go to BMC Bioinformatics",
                                 icon = icon("book"),
                                 time = "Published 2013-07-09",
                                 href = "https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-219"
                               ),
                               messageItem(
                                 from = "Source Code",
                                 message = "Available on Github",
                                 icon = icon("github"),
                                 time = "Update at 2018-10-09",
                                 href = "https://github.com/swsoyee/TCC-GUI"
                               ),
                               messageItem(
                                 from = "About Us",
                                 message = "Go to Lab Homepage",
                                 icon = icon("users"),
                                 href = "http://www.bi.a.u-tokyo.ac.jp/"
                               ),
                               icon = icon("info-circle"),
                               headerText = "INFORMATIONS"
                  )),
  dashboardSidebar(
    sidebarMenu(
      id = "sider",
      menuItem("Guidance", tabName = "guidence", icon = icon("home"),
               menuSubItem(text = "Welcome", tabName = "welcome"),
               menuSubItem(text = "1. Data import", tabName = "dataInputHelp"),
               menuSubItem(text = "2. TCC Computation", tabName = "tccComputationHelp"),
               tags$hr(style="border-color: black;"),
               menuSubItem(text = "3.1. MA plot", tabName = "maPlotHelp"),
               menuSubItem(text = "3.2. Volcano plot", tabName = "volcanoPlotHelp"),
               menuSubItem(text = "3.3. PCA analysis", tabName = "pcaHelp"),
               menuSubItem(text = "3.4. Heatmap", tabName = "heatmapHelp"),
               menuSubItem(text = "3.5. Expression level", tabName = "expressionHelp"),
               tags$hr(style="border-color: black;"),
               menuSubItem(text = "4. Output", tabName = "reportHelp"),
               startExpanded = TRUE),
      menuItem(
        "Simulation Data",
        tabName = "simulationData",
        icon = icon("database"),
        badgeLabel = "Step 0",
        badgeColor = "yellow"
      ), 
      menuItem(
        "Data Import",
        tabName = "dateImport",
        icon = icon("database"),
        badgeLabel = "Step 1",
        badgeColor = "yellow"
      ),
      menuItemOutput("calculationTab"),
      menuItemOutput("maplotTab"),
      menuItemOutput("volcanoplotTab"),
      menuItemOutput("pcaTab"),
      menuItemOutput("heatmapTab"),
      menuItemOutput("expressionTab"),
      menuItemOutput("reportTab")
    ),
    helpText("Copyright (c)2018 Bioinformation Engineering Lab, The University of Tokyo, All Rights Reserved. v2018.11.20", 
             style="padding-left:1em; padding-right:1em;position:absolute; bottom:1em; ")
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "welcome",
        box(
          title = "Introduction",
          solidHeader = TRUE,
          width = NULL,
          status = "info",
          includeMarkdown("document/English_Welcome.md")
        )
      ), 
      tabItem(tabName = "dataInputHelp", box(title = "Data import", 
                                             solidHeader = TRUE,
                                             width = NULL,
                                             status = "info",
                                             includeMarkdown("document/English_Data_input.md"))),
      tabItem(tabName = "tccComputationHelp", box(title = "TCC Computation",
                                                  solidHeader = TRUE,
                                                  width = NULL,
                                                  status = "info",
                                                  includeMarkdown("document/English_Computation.md"))),
      tabItem(tabName = "maPlotHelp", box(title = "MA Plot",
                                          solidHeader = TRUE,
                                          width = NULL,
                                          status = "info",
                                          includeMarkdown("document/English_MA_plot.md"))),
      tabItem(tabName = "volcanoPlotHelp", box(title = "Volcano Plot",
                                               solidHeader = TRUE,
                                               width = NULL,
                                               status = "info",
                                               includeMarkdown("document/English_Volcano_plot.md"))),
      tabItem(tabName = "pcaHelp", box(title = "PCA Analysis",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       status = "info",
                                       includeMarkdown("document/English_PCA_analysis.md"))),
      tabItem(tabName = "heatmapHelp", box(title = "Heatmap",
                                           solidHeader = TRUE,
                                           width = NULL,
                                           status = "info",
                                           includeMarkdown("document/English_Heatmap.md"))),
      tabItem(tabName = "expressionHelp", box(title = "Expression Level Plot",
                                              solidHeader = TRUE,
                                              width = NULL,
                                              status = "info",
                                              includeMarkdown("document/English_Expression.md"))),
      tabItem(tabName = "reportHelp", box(title = "Users' Logs & Report",
                                          solidHeader = TRUE,
                                          width = NULL,
                                          status = "info",
                                          includeMarkdown("document/English_More_help.md"))),
      
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
      tabItem(tabName = "pcaTab", source(
        file = "ui-pca.R",
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
)
