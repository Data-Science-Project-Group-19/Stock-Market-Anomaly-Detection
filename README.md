# Stock Market Anomaly Detector

##  MTH208 Statistical Data Analysis Project
### Indian Institute of Technology Kanpur

**Live App:** https://mth208-project.shinyapps.io/proj/

## Project Description
Interactive dashboard for detecting anomalies in Indian stock market data using statistical methods including Z-score analysis, volume spike detection, and time series forecasting.

## Features
- Real-time anomaly detection
- Statistical testing (ADF, KPSS, Shapiro-Wilk)
- Correlation analysis
- ARIMA forecasting
- Interactive visualizations

## Files
- `app.R` - Main Shiny application
- `data_processor.R` - Data download and preprocessing script

## Installation
1. Clone this repository
2. Run `data_processor.R` to download stock data
3. Run `shiny::runApp()` to launch locally

## Technologies
- R, Shiny, Plotly, Tidyquant
- Statistical analysis and time series forecasting
