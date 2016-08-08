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
  tags$head(tags$script(src="https://use.fontawesome.com/15c2608d79.js")) # Import FontAwesome for icons

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

  # Insert the right number of plot output objects into the web page
  output$plots <- renderUI({
    plot_output_list <- lapply(1:v$counter, function(i) {
      absolutePanel(
        top = 220, left = 20, #width = 300,
        draggable = TRUE,
        style = "opacity: 0.92",
        absolutePanel(right=-60, actionButton(paste0("createTable",i), "", icon("plus-circle fa-2x"), style="border:none; color:#00bc8c; background-color:rgb(60,60,60)")), 
        wellPanel(
          #htmlOutput("sliceSelect"), # Drop-down menu
          selectInput(paste0("selectSlice",i), "Slice by:", names(d.Preview()), multiple=FALSE),
          DT::dataTableOutput(paste0('sliceTable',i))
        )
      )
    })
    do.call(tagList, plot_output_list) # Convert the list to a tagList - this is necessary for the list of items to display properly.
  })

  # Handle button inputs
  v <- reactiveValues(counter = 1L)
  observe({
    for (i in 1:v$counter) {
      observeEvent(input[[paste0("createTable",i)]], v$counter <- v$counter + 1L)  
    }
  })
  output$debug <- renderPrint({ print(v$counter) })

  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  #for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
  #  local({
  #    my_i <- i
  #    plotname <- paste("plot", my_i, sep="")
  #    plottitle <- paste("plottitle", my_i, sep="")
  #    tablename <- paste("tablename", my_i, sep="")

  #    output[[plotname]] <- renderPlot({plot(1:my_i, 1:my_i, xlim = c(1, max_plots), ylim = c(1, max_plots), main = paste("1:", my_i, ".  n is ", input$n, sep = ""))})
  #    output[[plottitle]] <- renderText({paste("1:", my_i, ".  n is ", input$n, sep = "")})
  #    output[[tablename]] <- renderDataTable({table(x = 1:my_i, y = 1:my_i)})
  #  })
  #}

  # Draggable table
  d.slice <- reactive({
    filteredData <- filterData(d.Preview(), NULL, NULL, NULL) # First table no need to filter
    sliceData(filteredData, input$selectSlice)
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
  
  # Cell-colouring
  #session$onFlushed(function() {
  #  session$sendCustomMessage(type='jsCode', list(value = script))
  #})
})

filterData <- function(parentData, parentSlice, selectedRows, parentField) {
  if (parentSlice == NULL) return(parentData)
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