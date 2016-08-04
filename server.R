library(shiny)
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
  
  ## EXPLORE
  
  # Draggable table
  d.slice <- reactive({
    data <- group_by_(d.Preview(), input$selectSlice) # Rate.Plan
    data <- summarise(data, freq=n())
    data
  })
  output$sliceTable <- DT::renderDataTable({
    DT::datatable(d.slice(), escape=FALSE, 
                  options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  })
  output$sliceSelect <- renderUI({
    selectInput("selectSlice", "Slice by:", names(d.Preview()), multiple=FALSE)
  })
  
  # Draggable table2
  selectedRows <- reactive({
    rows = input$sliceTable_rows_selected
    rows
  })
  d.slice2 <- reactive({
    data <- d.slice()[as.numeric(selectedRows()),]
    data
  })
  output$sliceTable2 <- DT::renderDataTable({
    DT::datatable(d.slice2(), selection="none", escape=FALSE, 
                  options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  })
  
})
