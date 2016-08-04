library(shiny)
library(shinythemes)

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
        absolutePanel(
          top = 220, left = 20, #width = 300,
          draggable = TRUE,
          style = "opacity: 0.92",
          htmlOutput("sliceSelect"), # Drop-down menu
          DT::dataTableOutput('sliceTable')
        ),
        absolutePanel(
          top = 220, right = 20, #width = 300,
          draggable = TRUE,
          style = "opacity: 0.92",
          htmlOutput("sliceSelect2"), # Drop-down menu
          DT::dataTableOutput('sliceTable2')
        )
        #verbatimTextOutput('debug')
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
