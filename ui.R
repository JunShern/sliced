library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sliced"),
  
  # Sidebar with controls
  sidebarLayout( position = "left",
    sidebarPanel( 
      tags$h4("Upload CSV file"),
      checkboxInput('header', 'Headers', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      fileInput('file1', '',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      htmlOutput("selectUI"), # Drop-down menu
      numericInput("obs", "Number of observations to view:", 10)
    ),
    
    
    # Main display body
    mainPanel(
      h3(textOutput("caption", container = span)),
      
      DT::dataTableOutput('contents'),
      
      verbatimTextOutput("overview")
      
    )
  )
))
