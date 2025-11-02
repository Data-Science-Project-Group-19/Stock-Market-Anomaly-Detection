## 💹 Stock Market Anomaly Detection System  
**Course:** MTH208 – Data Science Lab I  
**Institute:** Indian Institute of Technology Kanpur  
**Group 19**

---

## 🧠 Project Overview
This project develops an automated system for detecting stock market anomalies in major NSE-listed companies using statistical and time-series methods.  
It integrates **Z-score analysis**, **volume spike detection**, **correlation analysis**, and **ARIMA-based forecasting** within an interactive **R Shiny dashboard**.

The system identifies unusual market behavior (price or volume deviations) and provides users with real-time visualization tools for exploratory analysis and forecasting.

---
## ▶️ App Design and Interactivity

The project is implemented as an interactive R Shiny dashboard that allows real-time analysis and visualization.
Users can select different stocks, change the date range, and view statistical outputs dynamically.
The interface has multiple tabs:

Dashboard: shows stock prices, volume, and detected anomalies with interactive charts.

Statistical Tests: performs tests like ADF, KPSS, and Shapiro–Wilk to check normality and stationarity.

Correlation Analysis: displays correlation heatmaps and scatter plots to study stock relationships.

Forecasting: provides ARIMA-based short-term price forecasts with confidence intervals.

About: gives details about project goals, data, and methods.

The app combines preprocessing, analytics, and visualization in one integrated environment, making it easy to explore data and reproduce results.
---
## 📂 Features
- Real-time **anomaly detection** using Z-score and rolling volatility.  
- **Interactive dashboard** for stock selection, date filtering, and visualization.  
- **Statistical tests** for normality, stationarity, and volatility clustering.  
- **Correlation heatmaps** for portfolio diversification analysis.  
- **ARIMA-based forecasting** with uncertainty intervals.  
- Ethical data sourcing via the **Yahoo Finance API**.

---

## ⚙️ How to Run Locally

### 1. Clone or download the repository
```bash
git clone https://github.com/Data-Science-Project-Group-19
cd Stock-Market-Anomaly-Detection
```

### 2. Install dependencies in R
```R
install.packages(c(
  "shiny", "plotly", "tidyquant", "dplyr", "forecast", 
  "ggplot2", "tseries", "DT", "shinythemes", "lubridate"
))
```

### 3. Run the application
```R
library(shiny)
runApp("app.R")
```

---

## 🌐 Online Version
Access the live deployed version here:  
🔗 **[Live Shiny App](https://mth208-project.shinyapps.io/proj/)**

---

## 📊 Data Source
- **Yahoo Finance API** via the R package `tidyquant`  
- Time range: **January 2018 – Present**  
- Tickers analyzed: Reliance, TCS, Infosys, HDFC Bank, ICICI Bank, SBI, HUL, ITC, Bharti Airtel, and L&T  

All data used is publicly available and collected under ethical research practices.

---

## 📁 Repository Contents
| File / Folder | Description |
|----------------|-------------|
| `app.R` | Main Shiny app file (includes preprocessing + UI + server logic) |
| `www/` | Contains all images, charts, and static assets used in the app |
| `data/` | Stores preprocessed or cached data (optional) |
| `Ds_lab_1_report_final.pdf` | Final written project report |
| `README.md` | Documentation and execution instructions |

---

## 👥 Team Members
- **Manish Kumar Meena**  
- **Vritika**  
- **Chandramohan Kushwah**  
- **Sumit Sana**

---

## 🧾 References
1. Yahoo Finance (2024). Historical Market Data. [https://finance.yahoo.com/](https://finance.yahoo.com/)  
2. R tidyquant, forecast, and Shiny Documentation.  
3. Chandola, V., Banerjee, A., & Kumar, V. (2009). *Anomaly Detection: A Survey.* ACM Computing Surveys, 41(3), 1–58.  
4. James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). *An Introduction to Statistical Learning.* Springer.

---

## 📜 License
This project is developed solely for academic and educational use under the **IIT Kanpur Data Science Lab (MTH208)** course guidelines.

---
