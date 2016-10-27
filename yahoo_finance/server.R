library(quantmod)
library(magrittr)

shinyServer(function(input, output) {
  
  observeEvent(input$run, {
    ticker_char <- reactive({
      input$symb
    })
    ticker_vector <- reactive({
      ticker_char() %>% strsplit(split = ',') %>% unlist()
    })
    ticker_df <- data.frame()
    
    getSymbols()
    
    #for (i in ticker_vector()) {
      #data <- reactive({
        #getSymbols(i,
                   #src = "yahoo",
                   #from = input$date,
                   #to = input$date)
      #})
      #ticker <- i
      #date <- reactive({
        #input$date
      #})
      #close_price <- Cl(get(data()))[[1]]
      #df_to_bind <- data.frame(ticker = ticker, date = date(), close_price = close_price)
      #ticker_df <- rbind(ticker_df(), df_to_bind)
    #}
    
    output$table <- renderDataTable({
      data()
    })
    
    output$test <- renderPrint({
      ticker_vector()
    })
    
  })
  
})