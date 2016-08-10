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
        fluidRow(verbatimTextOutput('debug')),
        tags$div(uiOutput("allSliceBoxes"))
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
