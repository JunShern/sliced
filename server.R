library(shiny)
library(datasets)

shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=300*1024^2) # Allow for file uploads of up to 300MB
  
  # File upload
  d.Preview <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                        quote=input$quote)
    data
  })
  
  # Add column names to dropdown selector
  output$selectUI <- renderUI({ 
    selectInput("column", "Display columns:", colnames(d.Preview()), multiple=TRUE)
  })
  
  # Table of uploaded file
  output$contents <- renderTable({
    print(output$selectUI)
    head(d.Preview(), input$obs)
  })
  
  # Print an overview of the data
  output$overview <- renderPrint({
    dim(d.Preview())
    str(d.Preview())
  })
  
  
})
