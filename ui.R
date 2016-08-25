library(shiny)
library(shinyjs)
library(markdown)

# Define UI for dataset viewer application
shinyUI(fluidPage(title = "SLICED", theme = "bootstrap.css", useShinyjs(),
  #tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });'))),
  #tags$head(tags$script(src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js")), # Import jQuery 
  tags$head(tags$link(href='https://fonts.googleapis.com/css?family=Yellowtail', rel='stylesheet', type='text/css')),
  
  # Header with controls
  tags$h1("Sliced", id="mainTitle", style='float:center; text-align:center; margin:20px 0px 0px 0px'),

  # Main display body
  fluidRow(style="padding:5px",
    # Settings panel is conditional on fileSettingsButton
    conditionalPanel(condition = 'input.fileSettingsButton % 2 == 1',
      absolutePanel(
        top = 165, right = 10, width = 180,
        draggable = FALSE,
        #style = "background-color:rgba(255,255,255,0.9); color:black",
        tags$div(class="panel panel-warning",
          tags$div(class="panel-heading",
            tags$h4(class="panel-title", "CSV file settings")
          ),
          tags$div(class="panel-body",
            checkboxInput('header', 'Headers', TRUE),
            radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),',') , 
            radioButtons('quote', 'Quote', c(None='','Double Quote'='"','Single Quote'="'"),'"')
          )
        )
      )
    ),
    tabsetPanel(
      tabPanel(icon("fa fa-pie-chart fa-2x"), #"Explore",
        #fluidRow(verbatimTextOutput('debug'), style="padding:10px 20px 0px 20px"),
        tags$div(uiOutput("allSliceBoxes"), style="padding:20px")
      ),
      tabPanel(icon("fa fa-list-alt fa-2x"), style="padding:20px", #"Summary",
        verbatimTextOutput("structure"), 
        verbatimTextOutput("summary")
      ),
      tabPanel(icon("fa fa-eye fa-2x"), style="padding:20px", #"Quick View",
        fluidRow(style="padding:0px", column(12, wellPanel(htmlOutput("selectUI"))) ),
        fluidRow(style="padding:10px", column(12, DT::dataTableOutput('contents')) )
      ),
      tabPanel(icon("fa fa-question fa-2x"), style="padding:20px", #"Help"
        tags$div(
          tags$div(style="margin:0 auto",
            tags$img(style="float:left; margin:20px",
              src='https://raw.githubusercontent.com/JunShern/sliced/master/www/demo.gif'
            ),
            tags$div(style="float:left; max-width:400px; margin:20px",
              includeMarkdown("www/helpfile.md")
            )
          )
        )
      )
    )
    
  )
  
))
