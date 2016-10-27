# Extract market cap, 52-week high and 52-week low from Google Finance
google_finance_url <- "https://www.google.com/finance?q=AAPL&ei=a2YQWMGuL5CF0gSP4K-IBw"
html_doc <- read_html(google_finance_url)
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
