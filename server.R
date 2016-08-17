library(shiny)
library(DT)
library(dplyr)
library(lazyeval)

shinyServer(function(input, output, session) {

  ### FUNCTIONS ###

  newNode <- function(id, parentId, choices = NULL) {
    node <- list(
      parent = parentId, 
      children = list()
    )
    # Create the UI for this node
    createSliceBox(id, parentId, choices) 
    return(node)
  }

  createSliceBox <- function(id, parentId, choices) {
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
            wellPanel(class="well well-sm", id=paste0('well',id),
              selectInput(selectID, paste0("Table ", id, ", child of ", parentId, "."), c("-Select-",choices), multiple=FALSE),
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
      updateSelectInput(session, selectID, choices=c("-Select-",names(d.Preview())) ) 
    )
  }

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
    print(paste0("Beginning assasination of ", currentId))
    parentId <- sliceBox.tree$tree[[currentId]]$parent
    # # Kill all its children
    numChildren <- length(sliceBox.tree$tree[[currentId]]$children)
    if (numChildren != 0) {
      for (i in 1:numChildren) {
        childId <- sliceBox.tree$tree[[currentId]]$children[[i]]
        #sliceBox.tree$tree[[childId]] <- list()
        killNode(childId) # Recursive assasination
      }
    }
    print(paste0("Killed children of ", currentId))
    # Tell its parents it's dead
    # numChildren <- length(sliceBox.tree$tree[[parentId]]$children)
    # for (i in 1:numChildren) {
    #   if (sliceBox.tree$tree[[parentId]]$children[i] == currentId) {
    #     sliceBox.tree$tree[[parentId]]$children[i] = NULL
    #   }
    # }
    # print(paste0("Told ", currentId, "'s parents."))
    # Kill the node
    sliceBox.tree$tree[[currentId]] <- list() # Don't NULL the node because we need it to hold its index in the list
    print(paste0("Killed ", currentId))
    # Kill the UI (this takes care of all children UIs as well)
    containerDivID <- paste0('container', currentId, '_div')
    removeUI(selector = paste0('#',containerDivID))
    print(paste0("Killed UI of ", currentId))
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

  # File upload input on navbar
  insertUI(
    selector = "ul", 
    where = 'beforeEnd',
    ui = div(id='fileInputDiv',
      fileInput('file1', label=NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      actionButton('fileSettingsButton', '', icon("fa fa-cog fa-2x"), style='border:none; background-color:rgba(255,255,255,0)')
    )
  )
  removeUI(selector = "#file1_progress") # No need for file upload loading bar

  ## EXPLORE

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
      #d.Preview() # First node no filter
    })
  })

  #output$debug <- renderPrint(sliceBox.tree$tree[[1]]$data.selected())
  # Keep a total count of all the button presses (also used loosely as the number of tables created)
  v <- reactiveValues(counter = 1L) 
  table <- reactiveValues(selectedRows = list())
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

      # Remember the rows being selected so that we don't lose this when re-rendering the data tables
      #output$debug <- renderPrint({
      for (i in 1:v$counter) {
        # Save the current state of rows
        rowstate <- isolate(input[[paste0("sliceBoxTable", i, "_rows_selected")]]) # Isolate to save only a snapshot of the rowstate 
        #print(roes)
        # Re-select these rows on next render
        tableID <- paste0("sliceBoxTable", i)
        proxy <- dataTableProxy(tableID) # use proxy to manipulate an existing table without completely re-rendering it
        proxy %>% selectRows(rowstate) 
      }
      #})

      # Filter the data based on selection
      sliceBox.data$display[[childId]] = reactive({
        sliceData(sliceBox.data$selected[[parentId]](), input[[paste0("sliceBoxSelect",childId)]])
      })
      sliceBox.data$selected[[childId]] = reactive({
        #selectedRows <- input[[paste0("sliceBoxTable", childId, "_rows_selected")]]
        filterData(sliceBox.data$selected[[parentId]](), sliceBox.data$display[[childId]](), 
          input[[paste0("sliceBoxTable", childId, "_rows_selected")]], input[[paste0("sliceBoxSelect",childId)]]) 
      })

      # Create new child
      sliceBox.tree$tree[[childId]] <- newNode(childId, parentId, choices=names(d.Preview()))

      # Append new childId to parent's list of children
      numChildren <- length(sliceBox.tree$tree[[parentId]]$children)
      sliceBox.tree$tree[[parentId]]$children[numChildren+1] <- childId 
    })

    # Event handler for close button
    observeEvent(input[[closeID]], {
      killNode(parentId)
    })
  })
  # output$debug <- renderPrint({
  #   # for (i in 2:v$counter) {
  #   #   print(table$selectedRows[[i]]())
  #   # }
  #   print(table$selectedRows[[2]]())
  # })
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

})

