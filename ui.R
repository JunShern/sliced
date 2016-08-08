library(shiny)
library(shinyjs)

# Define UI for dataset viewer application
shinyUI(fluidPage(title = "SLICED", theme = "bootstrap.css", useShinyjs(),
  #tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });'))),
  #tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js")), # Import jQuery for the Draggable

  # Header with controls
  fluidRow(
    column(2, #align="center",
      tags$h1("SLICED")
      #img(src='sliced_bread.jpg', width=170)
    ),
    column(10,
      fluidRow(
        column(4,
          tags$h4("Upload CSV file"),
          checkboxInput('header', 'Headers', TRUE),
          fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
        ),
        column(4,
          radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),',')  
        ),
        column(4,
          radioButtons('quote', 'Quote', c(None='','Double Quote'='"','Single Quote'="'"),'"')
        )
      )
    )
  ),
  
  # Main display body
  fluidRow(
    tabsetPanel(
      tabPanel("Explore",
        # absolutePanel(
        #   top = 220, left = 20, #width = 300,
        #   draggable = TRUE,
        #   style = "opacity: 0.92",
        #   absolutePanel(right=-60, actionButton("createTable", "", icon("plus-circle fa-2x"), style="border:none; color:#00bc8c; background-color:rgb(60,60,60)")), 
        #   wellPanel(
        #     htmlOutput("sliceSelect"), # Drop-down menu
        #     DT::dataTableOutput('sliceTable')
        #   )
        # ),
        # absolutePanel(
        #   top = 220, left = 400, #width = 300,
        #   draggable = TRUE,
        #   style = "opacity: 0.92",
        #   htmlOutput("sliceSelect2"), # Drop-down menu
        #   DT::dataTableOutput('sliceTable2')
        # ),
        uiOutput("plots"),
        verbatimTextOutput('debug')
      ),
      tabPanel("Summary",
               verbatimTextOutput("structure"), 
               verbatimTextOutput("summary")
      ),
      tabPanel("Quick View", 
        fluidRow(
          column(9, wellPanel(htmlOutput("selectUI")) ),
          column(3, numericInput("obs", "Number of observations to show:", 10))
        ),
        DT::dataTableOutput('contents') 
      )
    )
    
  )
  
))
