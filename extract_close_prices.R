# Load necessary packages
library(quantmod)
library(magrittr)
library(stringr)
library(RCurl)
library(jsonlite)
#library(xlsx)

# The quantmod part for close price
ExtractClosePrices <- function(specified_date, ticker_char) {
  ticker_vector <- ticker_char %>% strsplit(split = ',') %>% unlist()
  ticker_df <- data.frame()
  
  for (i in ticker_vector) {
    data <- getSymbols(i,
                       from = specified_date,
                       to = specified_date)
    close_price <- Cl(get(data))[[1]]
    df_to_rbind <- data.frame(ticker = i, date = specified_date, close_price = close_price)
    ticker_df <- rbind(ticker_df, df_to_rbind)
    #Sys.sleep(runif(1, 1, 2))
  }
  return(ticker_df)
}

# Extract Yahoo! Finance stats using JSON parser and regex parser
ScrapeYahooFinanceStats <- function(ticker_char) {
  ticker_vector <- ticker_char %>% strsplit(split = ',') %>% unlist()
  number_pattern <- "[0-9]+"
  digit_number_pattern <- "[0-9]+\\.[0-9]+"
  metric_df <- data.frame()
  
  for (i in ticker_vector) {
    # JSON parser
    json_url <- paste("https://query2.finance.yahoo.com/v10/finance/quoteSummary/", i, "?formatted=true&crumb=loFaprfreJS&lang=en-US&region=US&modules=defaultKeyStatistics%2CfinancialData%2CcalendarEvents&corsDomain=finance.yahoo.com", sep = '')
    json_object <- getURL(json_url)
    quote_summary <- fromJSON(json_object) %>% unlist()
    enterprise_value <- quote_summary["quoteSummary.result.defaultKeyStatistics.enterpriseValue.raw"] %>% unname() %>% as.numeric()
    enterprise_value_to_revenue <- quote_summary["quoteSummary.result.defaultKeyStatistics.enterpriseToRevenue.raw"] %>% unname() %>% as.numeric()
    enterprise_value_to_ebitda <- quote_summary["quoteSummary.result.defaultKeyStatistics.enterpriseToEbitda.raw"] %>% unname() %>% as.numeric()
    
    # regex parser
    text_url <- paste("https://finance.yahoo.com/quote/", i, "/key-statistics?p=", i, sep = '')
    text <- getURL(text_url)
    
    market_cap_pattern <- "\"marketCap\":\\{\"raw\":[0-9]+,"
    market_cap <- text %>%
      str_extract(pattern = market_cap_pattern) %>%
      str_extract(pattern = number_pattern) %>%
      unlist() %>%
      as.numeric()
    
    trailing_PE_pattern <- "\"trailingPE\":\\{\"raw\":[0-9]+\\.[0-9]+,\"fmt\":\"[0-9]+\\.[0-9]+\"\\}"
    trailing_PE_vector <- text %>%
      str_extract(pattern = trailing_PE_pattern) %>%
      str_extract_all(pattern = digit_number_pattern) %>%
      unlist()
    
    forward_PE_pattern <- "\"forwardPE\":\\{\"raw\":[0-9]+\\.[0-9]+,\"fmt\":\"[0-9]+\\.[0-9]+\"\\}"
    forward_PE_vector <- text %>%
      str_extract(pattern = forward_PE_pattern) %>%
      str_extract_all(pattern = digit_number_pattern) %>%
      unlist()
    
    fifty_two_week_high_pattern <- "\"fiftyTwoWeekHigh\":\\{\"raw\":[0-9]+\\.[0-9]+,\"fmt\":\"[0-9]+\\.[0-9]+\"\\}"
    fifty_two_week_high_vector <- text %>%
      str_extract(pattern = fifty_two_week_high_pattern) %>%
      str_extract_all(pattern = digit_number_pattern) %>%
      unlist()
    
    fifty_two_week_low_pattern <- "\"fiftyTwoWeekLow\":\\{\"raw\":[0-9]+\\.[0-9]+,\"fmt\":\"[0-9]+\\.[0-9]+\"\\}"
    fifty_two_week_low_vector <- text %>%
      str_extract(pattern = fifty_two_week_low_pattern) %>%
      str_extract_all(pattern = digit_number_pattern) %>%
      unlist()
    
    fifty_day_ma_pattern <- "\"fiftyDayAverage\":\\{\"raw\":[0-9]+\\.[0-9]+,\"fmt\":\"[0-9]+\\.[0-9]+\"\\}"
    fifty_day_ma_vector <- text %>%
      str_extract(pattern = fifty_day_ma_pattern) %>%
      str_extract_all(pattern = digit_number_pattern) %>%
      unlist()
    
    two_hundred_day_ma_pattern <- "\"twoHundredDayAverage\":\\{\"raw\":[0-9]+\\.[0-9]+,\"fmt\":\"[0-9]+\\.[0-9]+\"\\}"
    two_hundred_day_ma_vector <- text %>%
      str_extract(pattern = two_hundred_day_ma_pattern) %>%
      str_extract_all(pattern = digit_number_pattern) %>%
      unlist()
    
    forward_annual_divident_yield_pattern <- "\"dividendYield\":\\{\"raw\":[0-9]+\\.[0-9]+,\"fmt\":\"[0-9]+\\.[0-9]+%\"\\}"
    forward_annual_divident_yield_vector <- text %>%
      str_extract(pattern = forward_annual_divident_yield_pattern) %>%
      str_extract_all(pattern = digit_number_pattern) %>%
      unlist()
    
    trailing_PE <- trailing_PE_vector[1] %>% as.numeric()
    forward_PE <- forward_PE_vector[1] %>% as.numeric()
    fifty_two_week_high <- fifty_two_week_high_vector[1] %>% as.numeric()
    fifty_two_week_low <- fifty_two_week_low_vector[1] %>% as.numeric()
    fifty_day_ma <- fifty_day_ma_vector[1] %>% as.numeric()
    two_hundred_day_ma <- two_hundred_day_ma_vector[1] %>% as.numeric()
    forward_annual_divident_yield <- forward_annual_divident_yield_vector[1] %>% as.numeric()
    
    df_to_rbind <- data.frame(ticker = i,
                              enterprise_value = enterprise_value,
                              enterprise_value_to_revenue = enterprise_value_to_revenue,
                              enterprise_value_to_ebitda = enterprise_value_to_ebitda,
                              market_cap = market_cap,
                              trailing_PE = trailing_PE,
                              forward_PE = forward_PE,
                              fifty_two_week_high = fifty_two_week_high,
                              fifty_two_week_low = fifty_two_week_low,
                              fifty_day_ma = fifty_day_ma,
                              two_hundred_day_ma = two_hundred_day_ma,
                              forward_annual_divident_yield = forward_annual_divident_yield
                              )
    metric_df <- rbind(metric_df, df_to_rbind)
    Sys.sleep(runif(1, 1, 2))
  }
  return(metric_df)
}

# Function inputs
date <- "2016-10-26"
tickers <- "MSFT,AAPL,FB"

# Call function with user inputs
close_price <- ExtractClosePrices(specified_date = date, ticker_char = tickers)
other_metric <- ScrapeYahooFinanceStats(ticker_char = tickers)

# Merge on ticker
merged_data <- merge(close_price, other_metric)

# Export as csv or excel
date_for_filename <- date %>% str_replace_all(pattern = '-', replacement = '')
write.csv(merged_data, file = paste("yahoo_finance_", date_for_filename, '.csv', sep = ''), row.names = FALSE)
#write.xlsx(merged_data, file = paste("yahoo_finance_", date_for_filename, '.xls', sep = ''), row.names = FALSE)
