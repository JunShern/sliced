library(shiny)
library(datasets)

shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=300*1024^2) # Allow for file uploads of up to 300MB
  
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                        quote=input$quote)
    data
  })

  output$contents <- renderTable({
    myData()
  })
  
  output$selectUI <- renderUI({ 
    selectInput("column", "Columns", colnames(myData()))
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$caption <- renderText({
    input$caption
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
})
