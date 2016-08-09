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

  ### FUNCTIONS ###

  newNode <- function(id, parentId) {
    node <- list(
      parent = parentId, 
      children = list(),
      uiObject = createButton(id, parentId) #createSliceBox(id)
    )
    return(node)
  }

  createButton <- function(id, parentId) {
    buttonID <- paste0("sliceBoxButton", id)
    # Event handler for the button
    observeEvent(input[[buttonID]], {
      v$counter <- v$counter + 1L
      print(paste0("Pressed ", buttonID, ", parent is ", parentId))

      # Append new child to list of children
      numChildren <- length(sliceBox.tree[[id]]$children)
      sliceBox.tree[[id]]$children[numChildren + 1] <- v$counter 

      sliceBox.tree[v$counter] <- newNode(v$counter, id)
      output$debug <- renderPrint({ print(sliceBox.tree[[1]]) })
      })

    # Return the UI element for the button
    return(
      absolutePanel(left=100*id, actionButton(buttonID, paste0("I am ", buttonID, ", child of ", parentId), 
      icon("plus-circle fa-2x"), style="border:none; color:#00bc8c; background-color:rgb(60,60,60)")) 
      )
  }

  createSliceBox <- function(id, parentOutputData) {
    selectID <- paste0("sliceBoxSelect", id)
    tableID <- paste0("sliceBoxTable", id)
    buttonID <- paste0("sliceBoxButton", id)

    uiObject <- absolutePanel(
      top = 220, left = 20, #width = 300,
      draggable = TRUE,
      style = "opacity: 0.92",
      absolutePanel(right=-60, actionButton(input[[buttonID]], "", icon("plus-circle fa-2x"), style="border:none; color:#00bc8c; background-color:rgb(60,60,60)")), 
      wellPanel(
        #htmlOutput("sliceSelect"), # Drop-down menu
        selectInput(paste0("selectSlice",i), paste0("Table ", i, ". Slice by:"), names(d.Preview()), multiple=FALSE),
        DT::dataTableOutput(paste0('sliceTable',i))
      )
    )
    return(uiObject)
  }

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


  ### CODE STARTS HERE

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
  rootNode <- newNode(1, 0)
  sliceBox.tree <- list(rootNode) # We'll store our nodes as a 1D list, so parent and child ID's are recorded as their indices in the list

  # renderUI needs a list of uiObjects, so we just extract the uiObjects from every node of the tree
  output$allSliceBoxes <- renderUI({
    table_output_list <- lapply(1, function(i) {
      return(getElement(sliceBox.tree[[i]], 'uiObject'))
    })
    do.call(tagList, table_output_list) # Convert the list to a tagList - this is necessary for the list of items to display properly.
  })

  # Handle button inputs
  v <- reactiveValues(counter = 1L) # Keep a total count of all the button presses (also used loosely as the number of tables created)
  output$debug <- renderPrint({ print(sliceBox.tree[[1]]) })
  #observeEvent(input[["sliceBoxButton1"]], v$counter <- v$counter + 1L)
  # observeEvent(v$counter, {
  #   # Event handling for each button
  #   for (i in 1:v$counter) {
  #     buttonID <- paste0("sliceBoxButton", i)
  #     if (TRUE) { 
  #       print(paste0("Looking at ", i))
  #       observeEvent(input[[buttonID]], {
  #         # Upon button press
  #         v$counter <- v$counter + 1L
  #         parentId <- i # Parent is whichever button was pressed
  #         print(paste0("Pressed ", buttonID, ", parent is ", parentId))

  #         # Append new child to parent's list of children
  #         numChildren <- length(sliceBox.tree[[parentId]]$children)
  #         sliceBox.tree[[parentId]]$children[numChildren + 1] <- v$counter 

  #         sliceBox.tree[v$counter] <- newNode(v$counter, i)
  #         output$debug <- renderPrint({ print(sliceBox.tree[[1]]) })
  #         })
  #     }
  #     # # Table contents
  #     # d.slice <- reactive({
  #     #   #filteredData <- filterData(d.Preview(), NULL, NULL, NULL) # First table no need to filter
  #     #   sliceData(d.Preview(), input[[paste0("selectSlice",i)]])
  #     # })
  #     # output[[paste0("sliceTable",i)]] <- DT::renderDataTable({
  #     #   DT::datatable(d.slice(), 
  #     #     escape=FALSE, style = 'bootstrap', class = 'table-condensed table-bordered', 
  #     #     options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  #     # })

  #   }
  # })
  
  #output$debug <- renderPrint({ print(v$counter) })
  # Cell-colouring
  #session$onFlushed(function() {
  #  session$sendCustomMessage(type='jsCode', list(value = script))
  #})

})

