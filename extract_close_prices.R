library(quantmod)
library(magrittr)

extract_close_prices <- function(specified_date, ticker_char) {
  ticker_vector <- ticker_char %>% strsplit(split = ',') %>% unlist()
  ticker_df <- data.frame()
  
  for (i in ticker_vector) {
    data <- getSymbols(i,
                       from = specified_date,
                       to = specified_date)
    ticker <- i
    close_price <- Cl(get(data))[[1]]
    df_to_bind <- data.frame(ticker = ticker, date = specified_date, close_price = close_price)
    ticker_df <- rbind(ticker_df, df_to_bind)
  }
  return(ticker_df)
}

date <- "2016-10-21"
tickers <- "MSFT,AAPL"
result <- extract_close_prices(specified_date = date, ticker_char = tickers)

