library(shiny)

dashboardPage(
  dashboardHeader(title = "TCC GUI Version: Graphical User Interface for Tag Count Comparison (TCC) package", titleWidth = 800),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Guidence", tabName = "guidence", icon = icon("home")),
      menuItem("Data Import", tabName = "dateImport", icon = icon("database")),
      menuItemOutput("calculationTab"),
      menuItemOutput("maplotTab"),
      menuItemOutput("volcanoplotTab"),
      menuItemOutput("pcaTab"),
      menuItemOutput("heatmapTab"),
      menuItemOutput("expressionTab"),
      menuItemOutput("reportTab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "guidence", source(file = "ui-homepage.R",
                                           local = TRUE,
                                           encoding = "UTF-8"
                                           )$value
              ),
      tabItem(tabName = "dateImport", source(file = "ui-data-import.R",
                                           local = TRUE,
                                           encoding = "UTF-8"
      )$value
      ),
      tabItem(tabName = "calculationTab", source(file = "ui-calculation.R",
                                             local = TRUE,
                                             encoding = "UTF-8"
      )$value
      ),
      tabItem(tabName = "maplotTab", source(file = "ui-ma-plot.R",
                                              local = TRUE,
                                              encoding = "UTF-8"
      )$value
      ),
      tabItem(tabName = "volcanoplotTab", source(file = "ui-volcano-plot.R",
                                            local = TRUE,
                                            encoding = "UTF-8"
      )$value
      ),
      tabItem(tabName = "pcaTab", source(file = "ui-pca.R",
                                            local = TRUE,
                                            encoding = "UTF-8"
      )$value
      ),
      tabItem(tabName = "heatmapTab", source(file = "ui-heatmap.R",
                                            local = TRUE,
                                            encoding = "UTF-8"
      )$value
      ),
      # tabItem(tabName = "expressionTab", source(file = "ui-expression-plot.R",
      #                                       local = TRUE,
      #                                       encoding = "UTF-8"
      # )$value
      # ),
      tabItem(tabName = "reportTab", source(file = "ui-report.R",
                                            local = TRUE,
                                            encoding = "UTF-8"
      )$value
      )
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
