# server-tabPanel.R
#
observeEvent(input$TCC, {
  if (variables$runTimes <= 1) {
    appendTab(inputId = "tabs",
              tabPanel("MA Plot",
                       source(
                         file = "ui-ma-plot.R",
                         local = TRUE,
                         encoding = "UTF-8"
                       )$value))
    appendTab(inputId = "tabs",
              tabPanel("Volcano Plot",
                       source(
                         file = "ui-volcano-plot.R",
                         local = TRUE,
                         encoding = "UTF-8"
                       )$value))
    appendTab(inputId = "tabs",
              tabPanel("PCA",
                       source(
                         file = "ui-pca.R",
                         local = TRUE,
                         encoding = "UTF-8"
                       )$value))
    appendTab(inputId = "tabs",
              tabPanel("Heatmap",
                       source(
                         file = "ui-heatmap.R",
                         local = TRUE,
                         encoding = "UTF-8"
                       )$value))
    appendTab(inputId = "tabs",
              tabPanel("Expression",
                       source(
                         file = "ui-expression-plot.R",
                         local = TRUE,
                         encoding = "UTF-8"
                       )$value))
    # appendTab(
    #   inputId = "tabs",
    #   tabsetPanel("tabs",
    #   tabPanel("MA Plot",
    #            source(
    #              file = "ui-ma-plot.R",
    #              local = TRUE,
    #              encoding = "UTF-8"
    #            )$value),
    #   tabPanel("Volcano Plot",
    #            source(
    #              file = "ui-volcano-plot.R",
    #              local = TRUE,
    #              encoding = "UTF-8"
    #            )$value),
    #   tabPanel("PCA",
    #            source(
    #              file = "ui-pca.R",
    #              local = TRUE,
    #              encoding = "UTF-8"
    #            )$value),
    #   tabPanel("Heatmap",
    #            source(
    #              file = "ui-heatmap.R",
    #              local = TRUE,
    #              encoding = "UTF-8"
    #            )$value),
    #   tabPanel("Expression",
    #            source(
    #              file = "ui-expression-plot.R",
    #              local = TRUE,
    #              encoding = "UTF-8"
    #            )$value)
    # ))
  }
})