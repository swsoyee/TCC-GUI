library(shiny)
tagList(dashboardPage(
  dashboardHeader(
    title = "TCC-GUI: Graphical User Interface for TCC package",
    titleWidth = 500,
    dropdownMenu(
      type = "messages",
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
        time = "Update at 2018-12-13",
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
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sider",
      menuItem(
        "Guidance",
        tabName = "guidence",
        icon = icon("home"),
        menuSubItem(text = "Welcome", tabName = "welcome"),
        menuSubItem(text = "0. Simulation Data"),
        menuSubItem(text = "1. Exploratory Analysis", tabName = "dataInputHelp"),
        menuSubItem(text = "2. TCC Computation", tabName = "tccComputationHelp"),
        tags$hr(style = "border-color: black;"),
        menuSubItem(text = "3.1. MA Plot", tabName = "maPlotHelp"),
        menuSubItem(text = "3.2. Volcano Plot", tabName = "volcanoPlotHelp"),
        # menuSubItem(text = "3.3. PCA", tabName = "pcaHelp"),
        menuSubItem(text = "3.3. Heatmap", tabName = "heatmapHelp"),
        menuSubItem(text = "3.4. Expression Level", tabName = "expressionHelp"),
        tags$hr(style = "border-color: black;"),
        menuSubItem(text = "4. Output", tabName = "reportHelp"),
        startExpanded = TRUE
      ),
      menuItem(
        "Simulation Data",
        tabName = "simulationData",
        icon = icon("random"),
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
      # menuItemOutput("pcaTab"),
      menuItemOutput("heatmapTab"),
      menuItemOutput("expressionTab"),
      menuItemOutput("reportTab")
    )
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
      tabItem(
        tabName = "dataInputHelp",
        box(
          title = "Data import",
          solidHeader = TRUE,
          width = NULL,
          status = "info",
          includeMarkdown("document/English_Data_input.md")
        )
      ),
      tabItem(
        tabName = "tccComputationHelp",
        box(
          title = "TCC Computation",
          solidHeader = TRUE,
          width = NULL,
          status = "info",
          includeMarkdown("document/English_Computation.md")
        )
      ),
      tabItem(
        tabName = "maPlotHelp",
        box(
          title = "MA Plot",
          solidHeader = TRUE,
          width = NULL,
          status = "info",
          includeMarkdown("document/English_MA_plot.md")
        )
      ),
      tabItem(
        tabName = "volcanoPlotHelp",
        box(
          title = "Volcano Plot",
          solidHeader = TRUE,
          width = NULL,
          status = "info",
          includeMarkdown("document/English_Volcano_plot.md")
        )
      ),
      # tabItem(
      #   tabName = "pcaHelp",
      #   box(
      #     title = "Exploratory analysis (PCA)",
      #     solidHeader = TRUE,
      #     width = NULL,
      #     status = "info",
      #     includeMarkdown("document/English_PCA_analysis.md")
      #   )
      # ),
      tabItem(
        tabName = "heatmapHelp",
        box(
          title = "Heatmap",
          solidHeader = TRUE,
          width = NULL,
          status = "info",
          includeMarkdown("document/English_Heatmap.md")
        )
      ),
      tabItem(
        tabName = "expressionHelp",
        box(
          title = "Expression Level Plot",
          solidHeader = TRUE,
          width = NULL,
          status = "info",
          includeMarkdown("document/English_Expression.md")
        )
      ),
      tabItem(
        tabName = "reportHelp",
        box(
          title = "Analysis Report",
          solidHeader = TRUE,
          width = NULL,
          status = "info",
          includeMarkdown("document/English_More_help.md")
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
      # tabItem(tabName = "pcaTab", source(
      #   file = "ui-pca.R",
      #   local = TRUE,
      #   encoding = "UTF-8"
      # )$value),
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
  tags$p("Copyright © 2018"), 
  tags$a(" Bioinformation Engineering Lab, ", href = "http://www.bi.a.u-tokyo.ac.jp/"), 
  tags$a(" Graduate School of Agricultural and Life Sciences / Faculty of Agriculture, ", href = "http://www.a.u-tokyo.ac.jp/english/index.html"),
  tags$a(" The University of Tokyo ", href = "https://www.u-tokyo.ac.jp/en/index.html"), 
  tags$p("All Rights Reserved.  Version2018.12.13"),
  style = "
  bottom:0;
  width:100%;
  color: #B8C7CE;
  padding: 10px;
  background-color: #222D32;
  z-index: 1000;"
))