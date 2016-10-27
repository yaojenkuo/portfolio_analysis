library(quantmod)
library(magrittr)
library(rvest)
library(stringr)
library(RCurl)

# The quantmod part for close price
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
    #Sys.sleep()
  }
  return(ticker_df)
}

date <- "2016-10-21"
tickers <- "MSFT,AAPL"
result <- extract_close_prices(specified_date = date, ticker_char = tickers)

# The crawler part for other metrics
url <- "https://finance.yahoo.com/quote/AAPL/key-statistics?p=AAPL"
html_doc <- getURL(url)
fifty_two_week_high_pattern <- "\"fiftyTwoWeekHigh\":\\{\"raw\":[0-9]+\\.[0-9]+,\"fmt\":\"[0-9]+\\.[0-9]+\"\\}"
fifty_two_week_low_pattern <- "\"fiftyTwoWeekLow\":\\{\"raw\":[0-9]+\\.[0-9]+,\"fmt\":\"[0-9]+\\.[0-9]+\"\\}"

digit_number_pattern <- "[0-9]+\\.[0-9]+"
fifty_two_week_high <- html_doc %>%
  str_extract(pattern = fifty_two_week_high_pattern) %>%
  str_extract_all(pattern = digit_number_pattern) %>% 
  unlist() %>%
  as.numeric()

fifty_two_week_low <- html_doc %>%
  str_extract(pattern = fifty_two_week_low_pattern) %>%
  str_extract_all(pattern = digit_number_pattern) %>% 
  unlist() %>%
  as.numeric()

# Extract market cap, 52-week high and 52-week low from Google Finance
url <- "https://www.google.com/finance?q=AAPL&ei=a2YQWMGuL5CF0gSP4K-IBw"
html_doc <- read_html(url)
xpath_google_finance <- "//tr/td[@class='val']"

google_finance_metrics <- html_doc %>%
  html_nodes(xpath = xpath_google_finance) %>%
  html_text() %>%
  str_replace(pattern = "\\n", replacement = "")

market_cap <- google_finance_metrics[5]
fifty_two_week <- google_finance_metrics[2] %>%
  str_split(pattern = " - ") %>%
  unlist() %>%
  as.numeric()
fifty_two_week_high <- fifty_two_week[2]
fifty_two_week_low <- fifty_two_week[1]