# auto-install all missing packages
require_pkg <- c("tidyquant","dplyr","TTR","readr")
new_pkg <- require_pkg[!(require_pkg %in% installed.packages()[,"Package"])]
if(length(new_pkg)) install.packages(new_pkg)

# loading all the packages
library(tidyquant)
library(dplyr)
library(TTR)
library(readr)
# -------------------------------------------------------------
# Step 1: Download latest stock data and store a backup CSV
# -------------------------------------------------------------
stock_tickers <- c(
  "RELIANCE.NS", "TCS.NS", "HDFCBANK.NS", "INFY.NS", "ICICIBANK.NS",
  "HINDUNILVR.NS", "SBIN.NS", "BHARTIARTL.NS", "ITC.NS", "LT.NS"
)
start_date <- "2018-01-01"

# Ensure data folder exists
if (!dir.exists("data")) {
  dir.create("data")
}

# Fetch stock data and save a raw CSV backup
tq_get(stock_tickers, from = start_date) %>%
  write_csv("data/stock_data.csv")

# -------------------------------------------------------------
# Step 2: Process data – returns, Z-scores, and volume anomalies
# -------------------------------------------------------------
stock_data_processed <- read_csv("data/stock_data.csv", show_col_types = FALSE) %>%
  rename(
    Ticker = symbol,
    Date = date,
    Open = open,
    High = high,
    Low = low,
    Close = close,
    Volume = volume,
    Adjusted = adjusted
  ) %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Ticker) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    Return = (Close - lag(Close)) / lag(Close) * 100,
    RollingMean = SMA(Return, n = 20),
    RollingSD = runSD(Return, n = 20),
    Z_Score = (Return - RollingMean) / RollingSD,
    Anomaly = ifelse(abs(Z_Score) > 3, TRUE, FALSE),
    Vol_Avg = SMA(Volume, n = 20),
    Anomaly_Volume = ifelse(Volume > 2.5 * Vol_Avg, TRUE, FALSE)
  ) %>%
  ungroup()

# -------------------------------------------------------------
# Step 3: Save the processed dataset
# -------------------------------------------------------------
# The cleaned data is stored as an RDS file for quick loading in Shiny.
saveRDS(stock_data_processed, file = "data/processed_stock_data.rds")

print("Preprocessing complete. The 'processed_stock_data.rds' file is updated.")
