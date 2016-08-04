library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(theme = shinytheme("flatly"), #theme = "nightly.css"
  # Sidebar with controls
  fluidRow(
    column(2, align="center",
      tags$h1("Sliced")
      #img(src='sliced_bread.jpg', width=170)
    ),
    column(10,
           fluidRow(
             tags$h4("Upload CSV file")
           ),
           fluidRow(
             column(4,
                    checkboxInput('header', 'Headers', TRUE),
                    radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),',')  
             ),
             column(8,
                    radioButtons('quote', 'Quote', c(None='','Double Quote'='"','Single Quote'="'"),'"'),
                    fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
             )
           )
    )
  ),
  
  # Main display body
  fluidRow(
    tabsetPanel(
      tabPanel("Explore",
        absolutePanel(
          top = 300, left = 20, #width = 300,
          draggable = TRUE,
          style = "opacity: 0.92",
          htmlOutput("sliceSelect"), # Drop-down menu
          DT::dataTableOutput('sliceTable')
        )
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
