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
  fluidRow(style="padding:5px",
    tabsetPanel(
      tabPanel("Explore",
        fluidRow(verbatimTextOutput('debug'), style="padding:10px 20px 0px 20px"),
        tags$div(uiOutput("allSliceBoxes"), style="padding:20px")
      ),
      tabPanel("Summary", style="padding:20px",
        verbatimTextOutput("structure"), 
        verbatimTextOutput("summary")
      ),
      tabPanel("Quick View", style="padding:20px",
        fluidRow(style="padding:0px", column(12, wellPanel(htmlOutput("selectUI"))) ),
        fluidRow(style="padding:10px", column(12, DT::dataTableOutput('contents')) )
      )
    )
    
  )
  
))
