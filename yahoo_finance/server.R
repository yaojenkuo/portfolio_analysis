library(quantmod)
#source("helpers.R")

shinyServer(function(input, output) {
  
  data <- reactive({
    getSymbols(isolate(input$symb),
               src = "yahoo",
               from = isolate(input$date),
               to = isolate(input$date),
               auto.assign = FALSE)
  })
  
  observeEvent("run", {
    output$table <- renderDataTable(data())
  })
  
})