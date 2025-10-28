# Stock Market Anomaly Detector

##  MTH208 Statistical Data Analysis Project

### Indian Institute of Technology Kanpur

**Live App:** https://mth208-project.shinyapps.io/proj/

## 📁 File Structure & Purpose

### `APi 1.R` - DATA PROCESSOR
- **Purpose:** Downloads stock data from Yahoo Finance and preprocesses it
- **Function:** Calculates anomalies, Z-scores, rolling statistics, and volume spikes
- **Output:** Generates `data/processed_stock_data.rds` file
- **Important:** **RUN THIS FIRST** to download and prepare the data

### `Complete project DS lab 1.R` - MAIN APPLICATION  
- **Purpose:** Complete interactive Shiny dashboard
- **Function:** Anomaly detection, statistical tests, correlation analysis, ARIMA forecasting
- **Features:** Interactive plots, value boxes, downloadable data, heatmaps
- **Important:** **RUN THIS SECOND** to launch the web application

## 🚀 Installation & Usage

### Prerequisites
- R (version 4.3+ recommended)
- RStudio (optional but recommended)

### Step-by-Step Execution

1. **Clone this repository**
   ```bash
   git clone https://github.com/Data-Science-Project-Group-19.git
   cd Data-Science-Project-Group-19
