
# Stock Market Anomaly Detection System

## 📒 Statistical Analysis of Indian Stocks

A comprehensive R Shiny dashboard for detecting anomalies in Indian stock market data using statistical methods and machine learning approaches. Developed as part of the **MTH208 - Data Science Lab 1** course at **Indian Institute of Technology Kanpur**.

---

## 🎯 Project Overview

This project implements an automated anomaly detection system that identifies unusual market movements in real-time by analyzing historical stock data from 10 major Indian companies. The system provides investors with actionable insights through statistical validation, correlation analysis, and forecasting capabilities.

### Key Features:
- **Real-time Anomaly Detection**: Z-Score analysis for price anomalies and volume spike detection
- **Interactive Dashboard**: R Shiny application with dynamic visualizations
- **Statistical Validation**: Comprehensive testing (ADF, KPSS, Shapiro-Wilk, Jarque-Bera)
- **Portfolio Analysis**: Correlation heatmaps and diversification insights
- **Time Series Forecasting**: ARIMA models with confidence intervals
- **Sector-wise Analysis**: Comparative analysis across different market sectors

---

## 🗒️ Key Findings

### Anomaly Detection Performance:
- Banking stocks showed highest anomaly frequencies (ICICI Bank: 56, SBI: 61)
- FMCG stocks demonstrated lowest anomaly rates (ITC: 19, HUL: 23)
- Volume spikes frequently preceded significant price movements
- Strong sector-based correlation clustering observed

### Statistical Insights:
- COVID-19 period exhibited 3-5× normal volatility levels
- ARIMA models achieved 8-12% MAPE for 30-day forecasts
- Banking sector offers limited diversification benefits
- Defensive FMCG stocks provide valuable diversification during downturns

---

## 🛠️ Technical Implementation

### Methodology:
- **Z-Score Analysis**: Identifies abnormal returns beyond ±3 standard deviations using 20-day rolling windows
- **Volume Spike Detection**: Flags unusual trading activity (Volume > 2.5× 20-day average)
- **Volatility Monitoring**: 20-day rolling standard deviation of returns
- **Statistical Tests**: Stationarity (ADF, KPSS), Normality (Shapiro-Wilk, Jarque-Bera), Autocorrelation (ACF)
- **Time Series Forecasting**: Auto ARIMA with 95% confidence intervals

### Technologies Used:
- **R** with Shiny framework
- **tidyquant** for financial data from Yahoo Finance API
- **plotly** for interactive visualizations
- **forecast** for time series analysis
- **tseries** for statistical testing

---

## 📁 Dataset

**Source**: Yahoo Finance API via `tidyquant` package  
**Period**: January 2018 - Present  
**Coverage**: 10 major Indian companies across sectors:

| Sector | Companies |
|--------|-----------|
| Energy | Reliance Industries |
| IT | TCS, Infosys |
| Banking | HDFC Bank, ICICI Bank, State Bank of India |
| FMCG | Hindustan Unilever, ITC |
| Telecom | Bharti Airtel |
| Infrastructure | Larsen & Toubro |

---

## 🚀 Quick Start

### Prerequisites:
- R (version 4.3+)
- RStudio (recommended)

### Installation:
```r
# Install required packages
install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "readr", 
                   "tidyr", "tseries", "zoo", "forecast", "RcppRoll"))

# Run the application
shiny::runApp("Complete_project_DS_lab_1.R")
```

Live Application:

Access the deployed application: https://mth208-project.shinyapps.io/proj/

---

📈 Project Structure

```
├── Complete_project_DS_lab_1.R     # Main Shiny application
├── data/
│   └── processed_stock_data.rds    # Pre-processed stock data
├── docs/
│   └── Ds_lab_1_report__final_.pdf # Comprehensive project report
└── README.md
```

---

🔬 Research Questions Addressed

1. How can statistical methods systematically identify abnormal price movements and volume spikes?
2. What are key statistical properties of historical returns for major NSE stocks?
3. How can an interactive R Shiny dashboard implement real-time anomaly detection?
4. What correlation structures exist between major Indian stocks?
5. How effective are ARIMA models in providing short-term forecasts?
6. How do anomaly patterns vary across different market sectors?

---

👥 Contributors

Group 19 - IIT Kanpur

· Manish Kumar Meena
· Vritika 
· Chandramohan Kushwah 
· Sumit Sana 

Course: MTH208 - Data Science Lab 1
Institution: Indian Institute of Technology Kanpur
Supervisor: Dr. Akash Anand 

---

🔗 Links

· Live Application: ShinyApps.io

---

📚 References

1. Yahoo Finance API Documentation
2. R tidyquant Package Documentation
3. Shiny R Studio Documentation
4. Tsay, R. S. (2005). Analysis of Financial Time Series
5. Hyndman, R. J., & Athanasopoulos, G. (2018). Forecasting: Principles and Practice

---


© 2025 IIT Kanpur - Department of Mathematics & Statistics | MTH208 Data Science Lab 1