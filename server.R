library(shiny)
library(DT)
library(dplyr)
library(lazyeval)

## jQuery for table-colouring
script <- "$('tbody tr td:nth-child(5)').each(function() {

              var cellValue = $(this).text();

              if (cellValue > 50) {
                $(this).css('background-color', '#0c0');
              }
              else if (cellValue <= 50) {
                $(this).css('background-color', '#f00');
              }
            })"

shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=300*1024^2) # Allow for file uploads of up to 300MB
  tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js")) # Import jQuery for the Draggable

  # File upload
  d.Preview <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
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
                  style = 'bootstrap', class = 'table-condensed table-bordered', 
                  options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  })
  
  ## EXPLORE
  sliceTables <- list()

  # Draggable table
  d.slice <- reactive({
    sliceData(d.Preview(), input$selectSlice)
  })
  output$sliceTable <- DT::renderDataTable({
    DT::datatable(d.slice(), escape=FALSE, 
                  style = 'bootstrap', class = 'table-condensed table-bordered', 
                  options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  })
  output$sliceSelect <- renderUI({
    selectInput("selectSlice", "Slice by:", names(d.Preview()), multiple=FALSE)
  })
  
  # Draggable table2
  d.slice2 <- reactive({
    filteredData <- filterData(d.Preview(), d.slice(), input$sliceTable_rows_selected, input$selectSlice)
    sliceData(filteredData, input$selectSlice2)
  })
  output$sliceTable2 <- DT::renderDataTable({
    DT::datatable(d.slice2(), selection="none", escape=FALSE, 
                  style = 'bootstrap', class = 'table-condensed table-bordered', 
                  options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  })
  output$sliceSelect2 <- renderUI({
    selectInput("selectSlice2", "Slice by:", names(d.Preview()), multiple=FALSE)
  })
  
  # DEBUG
  output$debug <- renderPrint({
    field <- input$selectSlice
    values <- unlist( d.slice()[as.numeric(selectedRows()), field] )
    # Filter data
    expr <- lazyeval::interp(quote(x %in% y), x = as.name(field), y = values)
    data <- filter_(d.Preview(), expr)
    # Summarize data
    data <- group_by_(data, input$selectSlice2) # Rate.Plan
    data <- summarise(data, freq=n())
    data
  })
  
  # Cell-colouring
  #session$onFlushed(function() {
  #  session$sendCustomMessage(type='jsCode', list(value = script))
  #})
})

filterData <- function(parentData, parentSlice, selectedRows, parentField) {
  retainValues <- unlist(parentSlice[as.numeric(selectedRows), parentField])
  # Filter data to retain only rows containing (retainValues)
  expr <- lazyeval::interp(quote(x %in% y), x = as.name(parentField), y = retainValues)
  outputData <- filter_(parentData, expr)
  return(outputData)
}

sliceData <- function(filteredData, sliceField) {
  # Slice (summarize) data
  outputData <- group_by_(filteredData, sliceField) # Rate.Plan
  outputData <- summarise(outputData, Count=n())
  outputData <- mutate(outputData, Proportion=paste0(format(100*Count/sum(Count), nsmall=2), "%") )
  return(outputData)
}