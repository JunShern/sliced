library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sliced"),
  
  # Sidebar with controls
  sidebarLayout( position = "left",
    sidebarPanel( 
      tags$h4("Configure CSV file settings"),
      checkboxInput('header', 'Header', TRUE),
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
      fileInput('file1', 'Upload CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      textInput("caption", "Caption:", "Data Summary"),
      
      htmlOutput("selectUI"), #selectInput('columnNames', 'Columns', ""),
      
      numericInput("obs", "Number of observations to view:", 10)
      
    ),
    
    
    # Show the caption, a summary of the dataset and an HTML 
    # table with the requested number of observations
    mainPanel(
      h3(textOutput("caption", container = span)),
      
      #verbatimTextOutput("summary"), 
      
      #tableOutput("view")
      tableOutput('contents')
    )
  )
))
