# server-roku.R

observeEvent(input$runROKU, {
  req(input$runROKU)
  isolate({
    # Select Sample (Column)
    # Grouping.
    data.cl <- variables$groupListConvert
    
    # Using Original Dataset or Normalized Dataset.
    if (input$ROKUData == "o") {
      data <- variables$CountData[data.cl != 0]
    } else {
      data <- variables$norData
    }
    ROKUresult <- ROKU(data),
    
  })
})