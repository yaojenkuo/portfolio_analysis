library(shiny)

shinyUI(fluidPage(
  titlePanel("Yahoo! Finance Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select tickers and a specific date. 
               Information will be collected from Yahoo! Finance."),
      
      textInput("symb",
                label = "Use comma to separate tickers:"),
      
      dateInput("date",
                label = "Date:"),
      
      br(),
      br(),
      
      actionButton("run",
                   label = "Run")
    ),
    mainPanel(
      dataTableOutput('table')
    )
  )
))