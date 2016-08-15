library(shiny)
library(DT)
library(dplyr)
library(lazyeval)

shinyServer(function(input, output, session) {

  ### FUNCTIONS ###

  newNode <- function(id, parentId) {

    # sliceBox.data$display[[id]] = reactive({
    #   sliceData(sliceBox.data$selected[[parentId]](), input[[paste0("sliceBoxSelect",id)]])
    # })
    # sliceBox.data$selected[[id]] = reactive({
    #   selectedRows <- input[[paste0("sliceBoxTable", id, "_rows_selected")]]
    #   filterData(sliceBox.data$selected[[parentId]](), sliceBox.data$display[[id]](), selectedRows, input[[paste0("sliceBoxSelect",id)]]) 
    # })

    node <- list(
      parent = parentId, 
      children = list()
    )

    createSliceBox(id, parentId) # Create the UI for this node
    return(node)
  }

  createSliceBox <- function(id, parentId) {
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
    selectID <- paste0("sliceBoxSelect", id)
    tableID <- paste0("sliceBoxTable", id)
    buttonID <- paste0("sliceBoxButton", id)
    closeID <- paste0("sliceBoxClose", id)

    # Create table for display
    output[[tableID]] <- DT::renderDataTable({
      DT::datatable(sliceBox.data$display[[id]](), 
        escape=FALSE, style = 'bootstrap', class = 'table-condensed table-bordered', 
        options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
    })

    # Insert the UI element for the node under the parent's children_div
    insertUI(
      selector = paste0('#',parentDivID), 
      where = 'afterBegin',
      ui = tagList(
        tags$div(id=containerDivID, style='float:left',
          tags$div(id=nodeDivID, style='float:left; margin: 5px; min-width:250px',
            actionButton(buttonID, "", 
              icon("plus-circle fa-1x"), style="float:right; border:none; color:#00bc8c; background-color:rgba(0,0,0,0)"),
            actionButton(closeID, "", 
              icon("fa fa-times"), style="float:right; border:none; color:#f39c12; background-color:rgba(0,0,0,0)"),
            wellPanel(class="well well-sm",
              selectInput(selectID, paste0("Table ", id, ", child of ", parentId, "."), c(''), multiple=FALSE),
              DT::dataTableOutput(tableID)
            )
          ),
          tags$div(id=childrenDivID, style='float:left') # Container for children, starts empty
        ),
        tags$br('')
      )
    )

    # Observer for selectors
    observe(
      updateSelectInput(session, selectID, choices=names(d.Preview()) ) 
    )
  }

  # createSliceBox <- function(id, parentOutputData) {
  #   selectID <- paste0("sliceBoxSelect", id)
  #   tableID <- paste0("sliceBoxTable", id)
  #   buttonID <- paste0("sliceBoxButton", id)

  #   uiObject <- absolutePanel(
  #     top = 220, left = 20, #width = 300,
  #     draggable = TRUE,
  #     style = "opacity: 0.92",
  #     absolutePanel(right=-60, actionButton(input[[buttonID]], "", icon("plus-circle fa-2x"), style="border:none; color:#00bc8c; background-color:rgb(60,60,60)")), 
  #     wellPanel(
  #       #htmlOutput("sliceSelect"), # Drop-down menu
  #       selectInput(paste0("selectSlice",i), paste0("Table ", i, ". Slice by:"), names(d.Preview()), multiple=FALSE),
  #       DT::dataTableOutput(paste0('sliceTable',i))
  #     )
  #   )
  #   return(uiObject)
  # }

  filterData <- function(parentData, parentSlice, selectedRows, parentField) {
    retainValues <- unlist(parentSlice[as.numeric(selectedRows), parentField])
    # Filter data to retain only rows containing (retainValues)
    expr <- lazyeval::interp(quote(x %in% y), x = as.name(parentField), y = retainValues)
    outputData <- filter_(parentData, expr)
    return(outputData)
  }

  sliceData <- function(filteredData, sliceField) {
    # Slice (summarize) data
    outputData <- group_by_(filteredData, sliceField) 
    outputData <- summarise(outputData, Count=n())
    outputData <- mutate(outputData, Proportion=paste0(format(100*Count/sum(Count), nsmall=2), "%") )
    return(outputData)
  }

  killNode <- function(currentId) {
    parentId <- sliceBox.tree$tree[[currentId]]$parent
    # Kill the UI (this takes care of all children UIs as well)
    containerDivID <- paste0('container', currentId, '_div')
    removeUI(selector = paste0('#',containerDivID))
    # # Kill all its children
    # numChildren <- length(sliceBox.tree$tree[[currentId]]$children)
    # if (numChildren != 0) {
    #   for (i in 1:numChildren) {
    #     childId <- sliceBox.tree$tree[[currentId]]$children[[i]]
    #     print(childId)
    #     sliceBox.tree$tree[[childId]] <- NULL
    #     #killNode(childId) # Recursive assasination
    #   }
    # }
    # Kill the node
    sliceBox.tree$tree[[currentId]] <- NULL
    # Tell its parents it's dead
    numChildren <- length(sliceBox.tree$tree[[parentId]]$children)
    for (i in 1:numChildren) {
      if (sliceBox.tree$tree[[parentId]]$children[1] == currentId) {
        sliceBox.tree$tree[[parentId]]$children[1] = NULL
      }
    }
  }


  ### CODE STARTS HERE
  options(shiny.maxRequestSize=300*1024^2) # Allow for file uploads of up to 300MB
  tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js")) # Import jQuery 
  tags$head(tags$script(src="https://code.jquery.com/ui/1.12.0/jquery-ui.min.js")) # Import jQuqery UI (used for popup dialog)
  tags$head(tags$script(src="https://use.fontawesome.com/15c2608d79.js")) # Import FontAwesome for icons

  # File upload
  d.Preview <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    } else {
      data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      return(data)
    }
  })

  ## EXPLORE
  #selectionFields <- reactive(names(d.Preview))
  # We'll store our nodes as a 1D list, so parent and child ID's are recorded as their indices in the list
  sliceBox.data <- reactiveValues(display=list(), selected=list())
  rootNode <- newNode(1, 0) # Page loads with NULL first node, before file is uploaded
  sliceBox.tree <- reactiveValues(tree=list(rootNode))
  # Special case for loading data into first node, needs reactive parentData - not the case for children nodes
  observeEvent(input$file1, {
    slice <- reactive({
      sliceData(d.Preview(), input$sliceBoxSelect1)
    })
    # Creating data for the first node
    sliceBox.data$display[[1]] <- reactive(slice())
    sliceBox.data$selected[[1]] = reactive({
      selectedRows <- input[[paste0("sliceBoxTable", 1, "_rows_selected")]]
      filterData(d.Preview(), sliceBox.data$display[[1]](), selectedRows, input[[paste0("sliceBoxSelect",1)]]) 
    })

    # output$debug <- renderPrint({
    #   print(sliceBox.data$selected[[1]]())
    # })
  })

  #output$debug <- renderPrint(sliceBox.tree$tree[[1]]$data.selected())

  # Keep a total count of all the button presses (also used loosely as the number of tables created)
  v <- reactiveValues(counter = 1L) 
  # Every time v$counter is increased, create new handler for the new button at id=v$counter
  observeEvent(v$counter, {
    parentId <- v$counter
    buttonID <- paste0("sliceBoxButton", parentId)
    closeID <- paste0("sliceBoxClose", parentId)

    # Button handlers to create new sliceBoxes
    observeEvent(input[[buttonID]], {
      v$counter <- v$counter + 1L
      childId <- v$counter 
      # Note that because the ObserveEvents are run separately on different triggers, (childId != parentId+1)

      # Filter the data based on selection
      # output$debug <- renderPrint({
      #   print(sliceBox.data$selected[[parentId]]())
      # }) 
      sliceBox.data$display[[childId]] = reactive({
        sliceData(sliceBox.data$selected[[parentId]](), input[[paste0("sliceBoxSelect",childId)]])
      })
      sliceBox.data$selected[[childId]] = reactive({
        selectedRows <- input[[paste0("sliceBoxTable", childId, "_rows_selected")]]
        filterData(sliceBox.data$selected[[parentId]](), sliceBox.data$display[[childId]](), selectedRows, input[[paste0("sliceBoxSelect",childId)]]) 
      })

      # Now deselect the rows
      tableID <- paste0("sliceBoxTable", parentId)
      #proxy <- dataTableProxy(tableID) # use proxy to manipulate an existing table without completely re-rendering it
      #proxy %>% selectRows(NULL)

      # Create new chil
      sliceBox.tree$tree[[childId]] <- newNode(childId, parentId)

      # Append new childId to parent's list of children
      numChildren <- length(sliceBox.tree$tree[[parentId]]$children)
      sliceBox.tree$tree[[parentId]]$children[numChildren+1] <- childId 

      selectID <- paste0("sliceBoxSelect", childId)
      print(paste0("We are updating ", selectID, "!!"))
      updateSelectInput(session, selectID, choices=c('Hello','World')) 
    })

    # Event handler for close button
    observeEvent(input[[closeID]], {
      killNode(parentId)
    })
  })
  #output$debug <- renderPrint(sliceBox.tree$tree)

  ## QUICK VIEW TAB
  # Add column names to dropdown selector
  output$selectUI <- renderUI({ 
    selectInput("column", "Select fields:", names(d.Preview()), multiple=TRUE)
  })
  # Table of uploaded file
  output$contents <- DT::renderDataTable({
    DT::datatable(d.Preview()[, input$column, drop = FALSE], 
                  selection="none", escape=FALSE, 
                  style = 'bootstrap', class = 'table-condensed table-bordered', 
                  options = list(paging=TRUE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  })
  
  # Print overviews of the data
  output$structure <- renderPrint({
    str(d.Preview())
  })
  output$summary <- renderPrint({
    summary(d.Preview())
  })

  # Table contents
  # d.slice <- reactive({
  #   #filteredData <- filterData(d.Preview(), NULL, NULL, NULL) # First table no need to filter
  #   sliceData(d.Preview(), input[[paste0("selectSlice",1)]])
  # })
  # output[[paste0("sliceTable",1)]] <- DT::renderDataTable({
  #   DT::datatable(d.slice(), 
  #     escape=FALSE, style = 'bootstrap', class = 'table-condensed table-bordered', 
  #     options = list(paging=FALSE, searching=FALSE, autoWidth=FALSE, info=FALSE))
  # })
})

