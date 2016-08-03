library(shiny)
library(datasets)
library(DT)
library(dplyr)

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
    selectInput("column", "Select fields:", names(d.Preview()), multiple=TRUE)
  })
  
  ## EXPLORE
  
  # Draggable table
  output$sliceTable <- DT::renderDataTable({
    d.slice <- group_by(d.Preview(), walk_ins)
    d.slice <- summarise(d.slice, freq=n())
    DT::datatable(d.slice, selection="none", escape=FALSE, 
                  options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  })
  
  ## QUICK VIEW 
  
  # Print overviews of the data
  output$structure <- renderPrint({
    str(d.Preview())
  })
  output$summary <- renderPrint({
    summary(d.Preview())
  })
  # Table of uploaded file
  output$contents <- DT::renderDataTable({
    DT::datatable(head(d.Preview(), input$obs)[, input$column, drop = FALSE], 
                  selection="none", escape=FALSE, 
                  options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  })
  
})
