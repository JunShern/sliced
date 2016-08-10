library(shiny)
library(DT)
library(dplyr)
library(lazyeval)

shinyServer(function(input, output, session) {

  ### FUNCTIONS ###

  newNode <- function(id, parentId) {
    node <- list(
      parent = parentId, 
      children = list()
    )
    createButton(id, parentId) # Create the UI for this node
    return(node)
  }

  createButton <- function(id, parentId) {
    # Div names
    containerDivID <- paste0('container',id,'_div')
    nodeDivID <- paste0('node',id,'_div')
    childrenDivID <- paste0('children',id,'_div')

    if (parentId == 0) { # Root node case
      parentDivID <- 'allSliceBoxes'
    } else {
      parentDivID <- paste0('children',parentId,'_div')
    }

    # Input names
    buttonID <- paste0("sliceBoxButton", id)

    # Insert the UI element for the node under the parent's children_div
    insertUI(
      selector = paste0('#',parentDivID), 
      where = 'beforeEnd',
      ui = tagList(
        tags$div(id=containerDivID, style='float:left',
          tags$div(id=nodeDivID, style='float:left; margin: 5px',
            actionButton(buttonID, paste0("I am ", buttonID, ", child of ", parentId), 
              icon("plus-circle fa-2x"), style="border:none; color:#00bc8c; background-color:rgb(60,60,60)")
          ),
          tags$div(id=childrenDivID, style='float:left') # Container for children, starts empty
        ),
        tags$br('')
      )
      

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
  tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js")) # Import jQuery 
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
  #sliceBox.tree <- list(rootNode) # We'll store our nodes as a 1D list, so parent and child ID's are recorded as their indices in the list
  sliceBox.tree <- reactiveValues(tree=list(rootNode))

  # output$debug <- renderPrint({
  #   print(paste0("Button presses: ", v$counter))
  #   print("FULL TREE")
  #   print(sliceBox.tree$tree)
  # })

  # Keep a total count of all the button presses (also used loosely as the number of tables created)
  v <- reactiveValues(counter = 1L) 
  
  # Every time v$counter is increased, create new handler for the new button at id=v$counter
  observeEvent(v$counter, {
    id <- v$counter
    buttonID <- paste0("sliceBoxButton", id)
    # Create new button handler
    observeEvent(input[[buttonID]], {
      v$counter <- v$counter + 1L
      #print(paste0("Pressed ", buttonID))

      # Append new child to list of children
      numChildren <- length(sliceBox.tree$tree[[id]]$children)
      sliceBox.tree$tree[[id]]$children[v$counter] <- v$counter 

      sliceBox.tree$tree[[v$counter]] <- newNode(v$counter, id)
      #print(paste0("Appending node to tree at index ", v$counter))
      #print(node)
      #print(sliceBox.tree$tree)
    })
  })

  # renderUI needs a list of uiObjects, so we just extract the uiObjects from every node of the tree
  #output$allSliceBoxes <- renderUI({})

  # # Table contents
  # d.slice <- reactive({
  #   #filteredData <- filterData(d.Preview(), NULL, NULL, NULL) # First table no need to filter
  #   sliceData(d.Preview(), input[[paste0("selectSlice",i)]])
  # })
  # output[[paste0("sliceTable",i)]] <- DT::renderDataTable({
  #   DT::datatable(d.slice(), 
  #     escape=FALSE, style = 'bootstrap', class = 'table-condensed table-bordered', 
  #     options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  # })
})

