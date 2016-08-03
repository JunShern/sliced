library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(theme = shinytheme("flatly"), #theme = "nightly.css"
  # Sidebar with controls
  fluidRow( 
    column(2,
      tags$h1("Sliced")
    ),
    column(2,
      wellPanel(
        tags$h4("Upload CSV file"),
        checkboxInput('header', 'Headers', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ',')
      )
    ),
    column(2,
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      fileInput('file1', '',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
    ),
    column(6,
      htmlOutput("selectUI"), # Drop-down menu
      numericInput("obs", "Number of observations to show:", 10)
    ),
    tags$hr()
  ),
  
  # Main display body
  fluidRow(
    tabsetPanel(
      tabPanel("Explore",
        absolutePanel(
          bottom = 20, left = 20, #width = 300,
          draggable = TRUE,
          DT::dataTableOutput('sliceTable'), 
          style = "opacity: 0.92"
        )
      ),
      tabPanel("Quick View", 
        verbatimTextOutput("structure"), 
        verbatimTextOutput("summary"),
        DT::dataTableOutput('contents') 
      )
    )
    
  )
  
))
