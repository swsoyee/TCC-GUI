library(shiny)

dashboardPage(
  dashboardHeader(title = "TCC GUI Version: Graphical User Interface for Tag Count Comparison (TCC) package", titleWidth = 800,
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
      menuItem("Guidence", tabName = "guidence", icon = icon("home"),
               menuSubItem(text = "1. Data input", tabName = "dataInputHelp"),
               menuSubItem(text = "2. TCC Computation", tabName = "tccComputationHelp"),
               menuSubItem(text = "3.1. MA Plot", tabName = "maPlotHelp"),
               menuSubItem(text = "3.2. Volcano plot", tabName = "volcanoPlotHelp"),
               menuSubItem(text = "3.3. PCA analysis", tabName = "pcaHelp"),
               menuSubItem(text = "3.4. Heatmap", tabName = "heatmapHelp"),
               menuSubItem(text = "3.5. Expression level", tabName = "expressionHelp"),
               menuSubItem(text = "4. Report", tabName = "reportHelp"),
               startExpanded = TRUE),
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
    helpText("Copyright (c)2018 Bioinformation Engineering Lab, The University of Tokyo, All Rights Reserved.", 
             style="padding-left:1em; padding-right:1em;position:absolute; bottom:1em; ")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dataInputHelp", includeMarkdown("document/English_Data_input.md")),
      tabItem(tabName = "tccComputationHelp", includeMarkdown("document/English_Computation.md")),
      tabItem(tabName = "maPlotHelp", includeMarkdown("document/English_MA_plot.md")),
      tabItem(tabName = "volcanoPlotHelp", includeMarkdown("document/English_Volcano_plot.md")),
      tabItem(tabName = "pcaHelp", includeMarkdown("document/English_PCA_analysis.md")),
      tabItem(tabName = "heatmapHelp", includeMarkdown("document/English_Heatmap.md")),
      tabItem(tabName = "expressionHelp", includeMarkdown("document/English_Expression.md")),
      tabItem(tabName = "reportHelp", includeMarkdown("document/English_More_help.md")),
      
      tabItem(tabName = "guidence", source(
        file = "ui-homepage.R",
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
# shinyUI(fluidPage(
#   useShinyalert(),
#   navbarPage(
#     "TCC GUI Version: Graphical User Interface for Tag Count Comparison (TCC) package",
#     # position = "fixed-top",
#     theme = shinytheme("cosmo"),
#     # TCC Narvbar
#     tabPanel("TCC",
#              tabsetPanel(
#                id = "tabs",
#                tabPanel("Guidance",
#                         source(
#                           file = "ui-homepage.R",
#                           local = TRUE,
#                           encoding = "UTF-8"
#                         )$value),
#                tabPanel("Computation",
#                         source(
#                           file = "ui-data-import.R",
#                           local = TRUE,
#                           encoding = "UTF-8"
#                         )$value)
#              )),
#     # Footer
#     tags$hr(),
#     tags$br(),
#     tags$br(),
#     tags$br(),
#     absolutePanel(
#       bottom = 0,
#       left = 0,
#       right = 0,
#       fixed = TRUE,
#       div(style = "padding: 10px; border-bottom: 1px solid #CCC; background: #222222; color: #FFFFFF; opacity: 0.9",
#           HTML(
#             markdownToHTML(
#               fragment.only = TRUE,
#               text = c(
#                 "Code available on Github: [TCC-GUI](https://github.com/swsoyee/TCC-GUI)
#                  Copyright (c)2018 Bioinformation Engineering Lab, Department of Biotechnology, Graduate School of Agricultural and Life Science, The University of Tokyo, All Rights Reserved.
#                 "
#               )
#               )
#               ))
#               )
#             )#navbarPage
#           ))
