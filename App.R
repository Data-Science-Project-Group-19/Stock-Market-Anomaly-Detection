# ---------------------------------------------
# Stock Market Anomaly Detector
# ---------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))
# auto-install all missing packages, so the app runs without hassle
required_pkgs <- c("shiny","shinydashboard","plotly","dplyr","readr","tidyr",
                   "tseries","zoo","forecast","RcppRoll")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)

# loading all the packages we'll use in the app
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(tidyr)
library(tseries)
library(zoo)
library(forecast)
library(RcppRoll)

# ---------------------------------------------
# UI part (this sets up the pages/tabs and controls)
# ---------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Anomaly Detector"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Statistical Tests", tabName = "stats", icon = icon("flask")),
      menuItem("Compare & Correlate", tabName = "compare", icon = icon("chart-line")),
      menuItem("Forecasting", tabName = "forecast", icon = icon("chart-area")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
      
      
    )
  ),
  dashboardBody(
    tabItems(
      # main dashboard page
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("totalAnomaliesBox", width = 4),
                valueBoxOutput("mostVolatileBox", width = 4),
                valueBoxOutput("avgReturnBox", width = 4)
              ),
              fluidRow(
                box(width = 3, title = "Controls", status = "primary", solidHeader = TRUE,
                    selectInput("ticker", "Select Company:", choices = NULL, selected = NULL),
                    dateRangeInput("daterange", "Select Date Range:", start = "2018-01-01", end = Sys.Date()),
                    downloadButton("downloadData", "Download Filtered Data"),
                    hr(),
                    uiOutput("adfTestResult")
                    
                ),
                box(width = 9, status = "primary", plotlyOutput("pricePlot", height = "500px"))
              ),
              # show anomalies in a table (full width so it's easier to see)
              fluidRow(
                box(width = 12, title = "Detected Anomalies", status = "warning", solidHeader = TRUE, height = "auto",
                    dataTableOutput("anomalyTable"))
              ),
              # extra charts below (volatility + return distribution)
              fluidRow(
                tabBox(width = 12, title = "Further Analysis",
                       tabPanel("Volatility", plotlyOutput("volatilityPlot", height = "480px")),
                       tabPanel("Return Distribution", plotlyOutput("returnDistPlot", height = "480px"))
                )
              )
      ),
      
      # stats tab (separate controls so it won't mess with dashboard filters)
      tabItem(tabName = "stats",
              fluidRow(
                box(width = 3, title = "Controls", status = "primary", solidHeader = TRUE,
                    selectInput("ticker_stats", "Select Company:", choices = NULL, selected = NULL),
                    dateRangeInput("daterange_stats", "Select Date Range:", start = "2018-01-01", end = Sys.Date())
                ),
                box(width = 9, status = "primary", solidHeader = TRUE, title = "Normality & Stationarity",
                    uiOutput("normalityStationarityUI"), height = "350px")
              ),
              fluidRow(
                box(width = 4, title = "Autocorrelation (ACF)", status = "info", solidHeader = TRUE,
                    plotlyOutput("acfPlot", height = "350px")),
                box(width = 8, title = "Volume Trend & Spikes", status = "info", solidHeader = TRUE,
                    plotlyOutput("volumeTrendPlot", height = "350px"))
              ),
              fluidRow(
                box(width = 12, title = "Interpretation & Notes", status = "warning", solidHeader = TRUE,
                    uiOutput("statNotes"),
                    hr(),
                    uiOutput("statsInsightSummary")
                )
              )
      ),
      
      # comparing two stocks + overall heatmap
      tabItem(tabName = "compare",
              fluidRow(
                box(width = 3, title = "Controls", status = "primary", solidHeader = TRUE,
                    selectInput("ticker1", "Select First Company:", choices = NULL),
                    selectInput("ticker2", "Select Second Company:", choices = NULL),
                    dateRangeInput("daterange_compare", "Select Date Range:", start = "2018-01-01", end = Sys.Date())
                ),
                box(width = 9, status = "primary",
                    h4("Price Comparison"), plotlyOutput("comparePlot", height = "300px"), br(),
                    h4("Return Correlation"), verbatimTextOutput("correlationValue")
                )
              ),
              fluidRow(
                box(width = 12, title = "Market-Wide Correlation Heatmap", status = "info", solidHeader = TRUE,
                    plotlyOutput("correlationHeatmap", height = "600px"))
              )
      ),
      
      # forecasting page (ARIMA)
      tabItem(tabName = "forecast",
              fluidRow(
                box(width = 3, title = "Forecast Controls", status = "primary", solidHeader = TRUE,
                    selectInput("forecast_ticker", "Select Company:", choices = NULL),
                    numericInput("forecast_horizon", "Days to Forecast:", value = 30, min = 7, max = 180),
                    actionButton("run_forecast", "Generate Forecast", icon = icon("play"))
                ),
                box(width = 9, title = "Time Series Forecast (ARIMA)", status = "info", solidHeader = TRUE,
                    plotlyOutput("forecastPlot", height = "500px")
                )
              )
      ),
      
      # about tab (project description etc.)
      # student note: just text explaining what we built and why
      tabItem(tabName = "about",
              fluidRow(
                box(width = 12, title = "About This Project", status = "info", solidHeader = TRUE,
                    
                    h3("📈 Stock Market Anomaly Detection Dashboard"),
                    p("This dashboard was developed as part of the MTH208 – Statistical Data Analysis course project 
                 at the Indian Institute of Technology Kanpur (IIT Kanpur). The project demonstrates how 
                 statistical methods can be applied to real-world financial data for anomaly detection and 
                 market behavior analysis."),
                    
                    hr(),
                    
                    h4("🎯 Objective"),
                    p("The primary goal of this project is to design an automated, data-driven system that detects 
                 unusual market behavior using historical stock data. By identifying abnormal price changes, 
                 volatility spikes, and trading volume surges, the system aims to assist investors and analysts 
                 in recognizing periods of heightened risk or opportunity."),
                    
                    hr(),
                    
                    h4("💾 Data Source & Scope"),
                    tags$ul(
                      tags$li("Data is obtained from Yahoo Finance using the ", tags$b("tidyquant"), " R package."),
                      tags$li("Covers 10 major companies listed on the National Stock Exchange (NSE) of India."),
                      tags$li("Includes daily Open, High, Low, Close, Volume, and Adjusted prices from ", tags$b("2018 to Present.")),
                      tags$li("Data is publicly available and used solely for educational and research purposes.")
                    ),
                    
                    hr(),
                    
                    h4("🧠 Methodology"),
                    p("The project integrates multiple statistical techniques to examine market patterns and detect anomalies:"),
                    tags$ul(
                      tags$li(tags$b("Z-Score Method:"), " Identifies abnormal returns that deviate more than ±3 standard deviations 
                        from the rolling mean of daily returns."),
                      tags$li(tags$b("Volume Spike Detection:"), " Flags unusual trading activity when volume exceeds 2.5× its 
                        20-day rolling average."),
                      tags$li(tags$b("Volatility Analysis:"), " Monitors 20-day rolling standard deviation of returns to capture 
                        sudden shifts in market stability."),
                      tags$li(tags$b("Stationarity Tests (ADF & KPSS):"), " Evaluate whether the return series maintains 
                        statistical consistency over time."),
                      tags$li(tags$b("Normality Tests (Shapiro–Wilk & Jarque–Bera):"), " Examine whether daily returns follow a 
                        normal distribution — important for modeling market risk."),
                      tags$li(tags$b("Autocorrelation Analysis (ACF):"), " Tests for serial correlation in returns to identify 
                        possible momentum or mean-reversion behavior among stocks."),
                      tags$li(tags$b("Correlation & Comparison Analysis:"), " Measures cross-stock relationships and 
                        co-movement patterns using Pearson correlation.")
                    ),
                    
                    hr(),
                    
                    h4("📊 Dashboard Features"),
                    tags$ul(
                      tags$li("Interactive selection of company and date range."),
                      tags$li("Dynamic visualization of price trends with anomaly markers."),
                      tags$li("Anomaly summary tables and downloadable data export."),
                      tags$li("Volatility and return distribution analysis for selected stocks."),
                      tags$li("Statistical test module for in-depth diagnostic evaluation."),
                      tags$li("Correlation heatmap for inter-stock relationships."),
                      tags$li("Actionable insights highlighting abnormal behavior and volatility spikes.")
                    ),
                    
                    hr(),
                    
                    h4("⚙️ Technical Implementation"),
                    p("This application is built entirely in R using the following frameworks and packages:"),
                    tags$ul(
                      tags$li(tags$b("Shiny"), " – for reactive web application development."),
                      tags$li(tags$b("shinydashboard"), " – for structured dashboard UI design."),
                      tags$li(tags$b("plotly"), " – for interactive visualizations."),
                      tags$li(tags$b("dplyr, tidyr, TTR"), " – for data manipulation and statistical computation."),
                      tags$li(tags$b("tseries"), " – for time series testing and financial analysis.")
                    ),
                    
                    hr(),
                    
                    h4("💡 Actionable Insights"),
                    p("By identifying when stock behavior deviates significantly from the norm, this system helps investors 
                 make informed decisions. It highlights when volatility, volume, or returns behave abnormally — often 
                 signaling market reactions to external events or potential opportunities."),
                    
                    hr(),
                    
                    h4("📚 Credits"),
                    p("Developed by students of the Department of Statistics and Data Science, IIT Kanpur, as a part of the 
                 MTH208 project under faculty supervision. This project is intended purely for educational purposes 
                 and not for financial trading or investment advice."),
                    
                    hr(),
                    
                    h4("🔍 Summary of Analytical Tests Implemented"),
                    tags$table(
                      tags$thead(
                        tags$tr(
                          tags$th("Category"), tags$th("Test / Method"), tags$th("Purpose")
                        )
                      ),
                      tags$tbody(
                        tags$tr(tags$td("Anomaly Detection"), tags$td("Z-Score"), tags$td("Detects abnormal returns.")),
                        tags$tr(tags$td("Volume Analysis"), tags$td("Volume Spike Method"), tags$td("Identifies sudden trading surges.")),
                        tags$tr(tags$td("Volatility Analysis"), tags$td("Rolling SD"), tags$td("Measures 20-day market variability.")),
                        tags$tr(tags$td("Stationarity"), tags$td("ADF & KPSS"), tags$td("Checks if return series is stationary.")),
                        tags$tr(tags$td("Normality"), tags$td("Shapiro–Wilk, Jarque–Bera"), tags$td("Tests for normal distribution of returns.")),
                        tags$tr(tags$td("Autocorrelation"), tags$td("ACF (Lags 1–30)"), tags$td("Detects serial correlation or predictability.")),
                        tags$tr(tags$td("Cross-Stock Analysis"), tags$td("Pearson Correlation, Heatmap"), tags$td("Measures inter-stock relationships."))
                      )
                    ),
                    
                    hr(),
                    p(em("© 2025 IIT Kanpur – Developed for academic use under MTH208 Statistical Data Analysis Project."))
                )
              )
      )
      
    )
  )
)

# ---------------------------------------------
# SERVER part (all the logic lives here)
# ---------------------------------------------
server <- function(input, output, session) {
  
  # reading the pre-processed RDS file reactively 
  if (!file.exists("data/processed_stock_data.rds")) {
    source("preprocess.R")
  }
  rds_path <- "data/processed_stock_data.rds"
  
  stock_data_rdr <- reactive({
    df <- readRDS("data/processed_stock_data.rds")
  })
  
  # small helper to make sure we always get a proper data frame/tibble
  stock_data <- reactive({
    df <- stock_data_rdr()
    # if nothing loaded, return empty tibble so app doesn't crash
    if (is.null(df) || nrow(df) == 0) return(tibble())
    df
  })
  
  # once data loads, fill the dropdowns everywhere
  observe({
    df <- stock_data()
    if (nrow(df) == 0) return()
    tickers <- sort(unique(df$Ticker))
    # update selects across tabs so they're in sync at start
    updateSelectInput(session, "ticker", choices = tickers, selected = tickers[1])
    updateSelectInput(session, "ticker1", choices = tickers, selected = tickers[1])
    updateSelectInput(session, "ticker2", choices = tickers, selected = ifelse(length(tickers) >= 2, tickers[2], tickers[1]))
    updateSelectInput(session, "ticker_stats", choices = tickers, selected = tickers[1])
  })
  
  # this is the filtered data used on the dashboard page
  filtered_data <- reactive({
    df <- stock_data()
    req(nrow(df) > 0)
    req(input$ticker, input$daterange)
    df %>% filter(Ticker == input$ticker, Date >= input$daterange[1], Date <= input$daterange[2]) %>% arrange(Date)
  })
  
  # top value boxes: counts/anecdotes for quick glance
  output$totalAnomaliesBox <- renderValueBox({
    df <- filtered_data()
    total_anom <- sum((df$Anomaly == TRUE) | (df$Anomaly_Volume == TRUE), na.rm = TRUE)
    valueBox(value = total_anom, subtitle = "Total Anomalies (Z-score or Volume)", icon = icon("exclamation-triangle"),
             color = ifelse(total_anom > 0, "red", "green"))
  })
  
  output$mostVolatileBox <- renderValueBox({
    df <- filtered_data()
    if (nrow(df) == 0 || all(is.na(df$RollingSD))) {
      valueBox("N/A", "Most volatile (max 20d SD)", icon = icon("chart-area"), color = "yellow")
    } else {
      idx <- which.max(df$RollingSD)
      vol_date <- df$Date[idx]
      vol_val <- round(df$RollingSD[idx], 3)
      valueBox(value = paste0(as.character(vol_date), " (", vol_val, ")"), subtitle = "Most volatile (max 20d SD)",
               icon = icon("bolt"), color = "purple")
    }
  })
  
  output$avgReturnBox <- renderValueBox({
    df <- filtered_data()
    avg_ret <- round(mean(df$Return, na.rm = TRUE), 3)
    valueBox(value = ifelse(is.na(avg_ret), "N/A", paste0(avg_ret, "%")), subtitle = "Average daily return",
             icon = icon("percent"), color = "blue")
  })
  
  # tiny ADF summary on the dashboard (just a quick read)
  output$adfTestResult <- renderUI({
    # df <- filtered_data()
    df <- isolate(filtered_data())
    returns <- na.omit(df$Return)
    if (length(returns) < 20) return(tags$p("Not enough data to test stationarity.", style = "color: grey;"))
    adf_res <- tryCatch(tseries::adf.test(returns), error = function(e) NULL)
    if (is.null(adf_res)) return(tags$p("ADF test failed.", style = "color: grey;"))
    if (adf_res$p.value < 0.05) {
      tagList(tags$p(icon("check-circle", style = "color: green;"), "ADF: Data likely stationary."), tags$small(paste("p =", round(adf_res$p.value, 4))))
    } else {
      tagList(tags$p(icon("times-circle", style = "color: red;"), "ADF: Data likely non-stationary."), tags$small(paste("p =", round(adf_res$p.value, 4))))
    }
  })
  
  # dashboard plots + anomaly table
  output$pricePlot <- renderPlotly({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available for this period."))
    p <- plot_ly(df, x = ~Date, y = ~Close, type = "scatter", mode = "lines", name = "Close", line = list(color = "steelblue"))
    if (any(df$Anomaly == TRUE, na.rm = TRUE)) p <- p %>% add_markers(data = df %>% filter(Anomaly == TRUE), x = ~Date, y = ~Close, name = "Z-Score Anomaly", marker = list(color = "red", size = 8))
    if (any(df$Anomaly_Volume == TRUE, na.rm = TRUE)) p <- p %>% add_markers(data = df %>% filter(Anomaly_Volume == TRUE), x = ~Date, y = ~Close, name = "Volume Spike", marker = list(color = "purple", size = 10, symbol = "triangle-up"))
    p %>% layout(title = paste("Stock Price of", input$ticker), xaxis = list(title = "Date"), yaxis = list(title = "Close"))
  })
  
  output$volatilityPlot <- renderPlotly({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available."))
    plot_ly(df, x = ~Date, y = ~RollingSD, type = "scatter", mode = "lines", name = "20d SD", line = list(color = "purple")) %>%
      layout(title = "20-Day Rolling Volatility (SD of Returns)", xaxis = list(title = "Date"), yaxis = list(title = "Standard Deviation"))
  })
  
  output$returnDistPlot <- renderPlotly({
    df <- filtered_data()
    plot_ly(df, x = ~Return, type = "histogram", marker = list(color = "seagreen")) %>%
      layout(title = "Distribution of Daily Returns", xaxis = list(title = "Return (%)"))
  })
  
  output$anomalyTable <- renderDataTable({
    df <- filtered_data()
    df %>% mutate(Volume_Spike = ifelse(is.na(Anomaly_Volume), FALSE, Anomaly_Volume)) %>%
      filter(Anomaly == TRUE | Volume_Spike == TRUE) %>%
      select(Date, Close, Return, Z_Score, Volume, Volume_Spike) %>%
      arrange(desc(abs(Z_Score)))
  }, options = list(pageLength = 10))
  
  output$downloadData <- downloadHandler(
    filename = function() paste0("anomaly_data_", input$ticker, "_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  # ---------------------------------------------
  # Stats tab (separate flow so we can analyze deeper)
  # ---------------------------------------------
  filtered_stats_data <- reactive({
    df <- stock_data()
    req(nrow(df) > 0)
    req(input$ticker_stats, input$daterange_stats)
    df %>% filter(Ticker == input$ticker_stats, Date >= input$daterange_stats[1], Date <= input$daterange_stats[2]) %>% arrange(Date)
  })
  
  # this builds a mini report for normality + stationarity
  output$normalityStationarityUI <- renderUI({
    df <- filtered_stats_data()
    returns <- na.omit(df$Return)
    if (length(returns) < 20) {
      return(tags$p("Not enough data (need ≥ 20 returns) for full tests.", style = "color: grey;"))
    }
    # ADF (checks stationarity with a different null than KPSS)
    adf_res <- tryCatch(tseries::adf.test(returns), error = function(e) NULL)
    # KPSS (complementary to ADF)
    kpss_res <- tryCatch(tseries::kpss.test(returns), error = function(e) NULL)
    # Normality tests: Shapiro (capped to 5000 for speed) + Jarque-Bera
    sample_returns <- if (length(returns) > 5000) sample(returns, 5000) else returns
    sw_res <- tryCatch(shapiro.test(sample_returns), error = function(e) NULL)
    jb_res <- tryCatch(tseries::jarque.bera.test(sample_returns), error = function(e) NULL)
    
    
    # assembling the UI bits depending on p-values
    tagList(
      tags$h5("Stationarity (ADF & KPSS)"),
      if (!is.null(adf_res)) {
        if (adf_res$p.value < 0.05) tags$p(icon("check-circle", style = "color: green;"), "ADF: Stationary", tags$small(paste("p =", round(adf_res$p.value, 4)))) 
        else tags$p(icon("times-circle", style = "color: red;"), "ADF: Non-stationary", tags$small(paste("p =", round(adf_res$p.value, 4))))
      } else tags$p("ADF test failed or insufficient variance.", style = "color: grey;"),
      if (!is.null(kpss_res)) {
        # KPSS null = stationary => p < 0.05 => reject stationarity
        if (kpss_res$p.value < 0.05) tags$p(icon("times-circle", style = "color: red;"), "KPSS: Non-stationary", tags$small(paste("p =", round(kpss_res$p.value, 4)))) 
        else tags$p(icon("check-circle", style = "color: green;"), "KPSS: Stationary", tags$small(paste("p =", round(kpss_res$p.value, 4))))
      } else tags$p("KPSS test failed.", style = "color: grey;"),
      hr(),
      tags$h5("Normality (Shapiro-Wilk & Jarque-Bera)"),
      if (!is.null(sw_res)) {
        if (sw_res$p.value < 0.05) tags$p(icon("times-circle", style = "color: red;"), "Shapiro-Wilk: NOT normal", tags$small(paste("W =", round(sw_res$statistic, 4), "p =", signif(sw_res$p.value, 4)))) 
        else tags$p(icon("check-circle", style = "color: green;"), "Shapiro-Wilk: approx. normal", tags$small(paste("W =", round(sw_res$statistic, 4), "p =", signif(sw_res$p.value, 4))))
      } else tags$p("Shapiro-Wilk failed (too large sample or error).", style = "color: grey;"),
      if (!is.null(jb_res)) {
        if (jb_res$p.value < 0.05) tags$p(icon("times-circle", style = "color: red;"), "Jarque-Bera: NOT normal", tags$small(paste("stat =", round(jb_res$statistic, 4), "p =", signif(jb_res$p.value, 4)))) 
        else tags$p(icon("check-circle", style = "color: green;"), "Jarque-Bera: approx. normal", tags$small(paste("stat =", round(jb_res$statistic, 4), "p =", signif(jb_res$p.value, 4))))
      } else tags$p("Jarque-Bera failed.", style = "color: grey;")
    )
  })
  
  # ACF plot for the stats tab
  output$acfPlot <- renderPlotly({
    df <- filtered_stats_data()
    returns <- na.omit(df$Return)
    validate(need(length(returns) > 30, "Not enough data for ACF"))
    acf_obj <- acf(tail(returns, 2000), lag.max = 30, plot = FALSE)
    lags <- as.numeric(acf_obj$lag)[-1]
    acfs <- as.numeric(acf_obj$acf)[-1]
    acf_df <- data.frame(lag = lags, acf = acfs)
    plot_ly(acf_df, x = ~lag, y = ~acf, type = "bar") %>%
      layout(title = "ACF of Returns (lags 1–30)", xaxis = list(title = "Lag"), yaxis = list(title = "ACF"))
  })
  
  # volume trend with spikes marked (helps validate anomalies)
  output$volumeTrendPlot <- renderPlotly({
    df <- filtered_stats_data() %>%
      mutate(Vol_Avg = zoo::rollmean(Volume, k = 20, fill = NA, align = "right"),
             Volume_Spike = ifelse(!is.na(Vol_Avg) & Volume > 2.5 * Vol_Avg, TRUE, FALSE))
    validate(need(nrow(df) > 0, "No data for volume trend"))
    p <- plot_ly(df, x = ~Date, y = ~Volume, type = "scatter", mode = "lines", name = "Volume", line = list(color = "orange"))
    p <- p %>% add_lines(data = df, x = ~Date, y = ~Vol_Avg, name = "20-day Avg", line = list(color = "black", dash = "dash"))
    if (any(df$Volume_Spike == TRUE, na.rm = TRUE)) {
      p <- p %>% add_markers(data = df %>% filter(Volume_Spike == TRUE), x = ~Date, y = ~Volume, name = "Vol Spike", marker = list(color = "purple", symbol = "triangle-up", size = 8))
    }
    p %>% layout(title = paste("Volume Trend & Spikes -", input$ticker_stats), xaxis = list(title = "Date"), yaxis = list(title = "Volume"))
  })
  
  # some short guidance text for the stats tab
  output$statNotes <- renderUI({
    tagList(
      p("This page runs more detailed statistical diagnostics on the selected stock and date range."),
      tags$ul(
        tags$li("Use Shapiro-Wilk and Jarque-Bera to evaluate normality of returns (most financial returns are non-normal)."),
        tags$li("ADF & KPSS jointly provide evidence whether a series is stationary (results can sometimes conflict)."),
        tags$li("ACF / Ljung-Box help detect serial correlation (market inefficiency or mean reversion)."),
        tags$li("Volume spikes indicate heightened market participation and help validate price anomalies.")
      )
    )
  })
  
  # summary paragraph that stitches together the test results
  output$statsInsightSummary <- renderUI({
    df <- filtered_stats_data()
    if (nrow(df) == 0) return(tags$p("No data in selected range.", style = "color: grey;"))
    returns <- na.omit(df$Return)
    # stationarity
    adf_res <- tryCatch(tseries::adf.test(returns), error = function(e) NULL)
    kpss_res <- tryCatch(tseries::kpss.test(returns), error = function(e) NULL)
    # normality
    sw_res <- tryCatch(shapiro.test(if (length(returns)>5000) sample(returns,5000) else returns), error = function(e) NULL)
    jb_res <- tryCatch(tseries::jarque.bera.test(returns), error = function(e) NULL)
    # autocorr
    lb_res <- tryCatch(Box.test(returns, lag = 20, type = "Ljung-Box"), error = function(e) NULL)
    # anomalies & volume spikes
    total_anom <- sum((df$Anomaly == TRUE) | (df$Anomaly_Volume == TRUE), na.rm = TRUE)
    vol_spikes <- sum(df$Anomaly_Volume == TRUE, na.rm = TRUE)
    last_anom_date <- if (total_anom > 0) max(df$Date[(df$Anomaly == TRUE) | (df$Anomaly_Volume == TRUE)], na.rm = TRUE) else NA
    # volatility check (compare recent to overall)
    recent_sd <- mean(tail(na.omit(df$RollingSD), 20), na.rm = TRUE)
    hist_sd <- mean(na.omit(df$RollingSD), na.rm = TRUE)
    vol_flag <- if (!is.na(recent_sd) && !is.na(hist_sd) && recent_sd > 1.5 * hist_sd) TRUE else FALSE
    # quick narrative summary
    parts <- c()
    parts <- c(parts, paste0("Total anomalies in range: ", total_anom, " (volume spikes: ", vol_spikes, ")."))
    if (!is.na(last_anom_date)) parts <- c(parts, paste0("Last anomaly: ", as.character(last_anom_date), "."))
    # stationarity summary
    st_msg <- "Stationarity: "
    if (!is.null(adf_res) && !is.null(kpss_res)) {
      adf_stat <- ifelse(adf_res$p.value < 0.05, "ADF suggests stationary", "ADF suggests non-stationary")
      kpss_stat <- ifelse(kpss_res$p.value < 0.05, "KPSS suggests non-stationary", "KPSS suggests stationary")
      st_msg <- paste(st_msg, adf_stat, " / ", kpss_stat, ".")
    } else {
      st_msg <- paste(st_msg, "Tests incomplete.")
    }
    parts <- c(parts, st_msg)
    # normality
    norm_msg <- "Normality: "
    if (!is.null(sw_res) || !is.null(jb_res)) {
      sw_flag <- ifelse(!is.null(sw_res) && sw_res$p.value < 0.05, TRUE, FALSE)
      jb_flag <- ifelse(!is.null(jb_res) && jb_res$p.value < 0.05, TRUE, FALSE)
      if (sw_flag || jb_flag) norm_msg <- paste0(norm_msg, "Returns are NOT normally distributed (fat tails).") else norm_msg <- paste0(norm_msg, "Returns approx. normal.")
    } else norm_msg <- paste0(norm_msg, "Tests unavailable.")
    parts <- c(parts, norm_msg)
    # autocorr
    ac_msg <- "Autocorrelation: "
    if (!is.null(lb_res)) {
      if (lb_res$p.value < 0.05) ac_msg <- paste0(ac_msg, "Significant autocorrelation detected (Ljung-Box p=", round(lb_res$p.value,4), ").") else ac_msg <- paste0(ac_msg, "No significant autocorrelation (Ljung-Box p=", round(lb_res$p.value,4), ").")
    } else ac_msg <- paste0(ac_msg, "Test unavailable.")
    parts <- c(parts, ac_msg)
    # volatility
    if (vol_flag) parts <- c(parts, "Recent volatility is significantly higher than historical (caution).") else parts <- c(parts, "Recent volatility close to historical average.")
    # final tip (just a generic student recommendation)
    rec <- "Recommendation: Use detected anomalies as alerts to investigate news/earnings; combine price+volume signals before acting. Non-normal returns and elevated volatility suggest increased tail risk; prefer risk controls."
    parts <- c(parts, rec)
    tags$p(paste(parts, collapse = " "), style = "font-weight:bold;")
  })
  
  # ---------------------------------------------
  # Compare & Correlate (pairwise + heatmap)
  # ---------------------------------------------
  compare_data <- reactive({
    df <- stock_data()
    req(nrow(df) > 0)
    req(input$ticker1, input$ticker2, input$daterange_compare)
    df1 <- df %>% filter(Ticker == input$ticker1, Date >= input$daterange_compare[1], Date <= input$daterange_compare[2])
    df2 <- df %>% filter(Ticker == input$ticker2, Date >= input$daterange_compare[1], Date <= input$daterange_compare[2])
    list(df1 = df1, df2 = df2)
  })
  
  output$comparePlot <- renderPlotly({
    dfs <- compare_data()
    validate(need(nrow(dfs$df1) > 0 && nrow(dfs$df2) > 0, "No data available."))
    plot_ly(dfs$df1, x = ~Date, y = ~Close, type = "scatter", mode = "lines", name = input$ticker1, line = list(color = "steelblue")) %>%
      add_trace(data = dfs$df2, x = ~Date, y = ~Close, mode = "lines", name = input$ticker2, line = list(color = "orange")) %>%
      layout(title = paste("Price Comparison:", input$ticker1, "vs", input$ticker2))
  })
  
  output$correlationValue <- renderText({
    dfs <- compare_data()
    merged <- inner_join(dfs$df1, dfs$df2, by = "Date", suffix = c("_1", "_2"))
    if (nrow(merged) < 2) return("Not enough common data points to calculate correlation.")
    corr <- cor(merged$Return_1, merged$Return_2, use = "complete.obs")
    paste("Correlation of Daily Returns:", round(corr, 3))
  })
  
  output$correlationHeatmap <- renderPlotly({
    df <- stock_data()
    validate(need(nrow(df) > 0, "No data loaded."))
    returns_wide <- df %>% select(Date, Ticker, Return) %>% pivot_wider(names_from = Ticker, values_from = Return)
    cor_matrix <- cor(returns_wide %>% select(-Date), use = "pairwise.complete.obs")
    cor_matrix[is.na(cor_matrix)] <- 0
    plot_ly(x = colnames(cor_matrix), y = rownames(cor_matrix), z = cor_matrix, type = "heatmap", colorscale = "Viridis") %>%
      layout(title = "Correlation Matrix of Daily Returns (All Stocks)")
  })
  
  # ---------------------------------------------
  # FORECASTING 
  # ---------------------------------------------
  
  # keeping the forecast ticker list updated once data is present
  observe({
    df <- stock_data()
    if (nrow(df) > 0) {
      updateSelectInput(session, "forecast_ticker", choices = sort(unique(df$Ticker)))
    }
  })
  
  observeEvent(input$run_forecast, {
    # subset data
    df <- stock_data() %>% filter(Ticker == input$forecast_ticker)
    returns <- na.omit(df$Close)
    model <- auto.arima(returns)
    forecasted <- forecast(model, h = input$forecast_horizon)
    output$forecastPlot <- renderPlotly({
      plot_ly() %>% 
        add_lines(x = df$Date, y = returns, name = "Historical") %>%
        add_lines(x = seq(max(df$Date) + 1, by = "day", length.out = input$forecast_horizon),
                  y = forecasted$mean, name = "Forecast", line = list(color = "red"))
    })
  })
  
  # running the forecast when user clicks the button
  forecast_results <- eventReactive(input$run_forecast, {
    req(input$forecast_ticker)
    df <- stock_data() %>% filter(Ticker == input$forecast_ticker) %>% arrange(Date)
    validate(need(nrow(df) > 50, "Not enough historical data to generate a reliable forecast."))
    
    # progress bar so it doesn't look frozen
    withProgress(message = 'Generating forecast...', value = 0, {
      
      # time series from closing prices
      ts_data <- ts(df$Close)
      
      # auto.arima can be a bit slow; turning on stepwise/approx for speed
      setProgress(0.5, detail = "Fitting ARIMA model...")
      fit <- forecast::auto.arima(ts_data, stepwise = TRUE, approximation = TRUE)
      
      
      # make the forecast
      fc <- forecast(fit, h = input$forecast_horizon)
      setProgress(1, detail = "Done.")
      
      # return both the raw data and the forecast object
      list(data = df, forecast = fc)
    })
  })
  
  # draw the forecast plot with confidence ribbons
  output$forecastPlot <- renderPlotly({
    res <- forecast_results()
    
    # build a small data frame for plotting the forecast values
    forecast_df <- data.frame(
      Date = tail(res$data$Date, 1) + 1:input$forecast_horizon,
      Forecast = as.numeric(res$forecast$mean),
      Lower_CI = as.numeric(res$forecast$lower[, 2]), # 95% confidence interval
      Upper_CI = as.numeric(res$forecast$upper[, 2])  # 95% confidence interval
    )
    
    plot_ly() %>%
      # historical series
      add_lines(data = res$data, x = ~Date, y = ~Close, name = "Historical Data") %>%
      # forecast line
      add_lines(data = forecast_df, x = ~Date, y = ~Forecast, name = "Forecast", line = list(dash = 'dash')) %>%
      # confidence ribbon
      add_ribbons(data = forecast_df, x = ~Date, ymin = ~Lower_CI, ymax = ~Upper_CI, name = "95% Confidence Interval") %>%
      layout(title = paste("ARIMA Forecast for", input$forecast_ticker),
             xaxis = list(title = "Date"), yaxis = list(title = "Closing Price (₹)"))
  })
}

# ---------------------------------------------
# start the app (ui + server)
# ---------------------------------------------
shinyApp(ui = ui, server = server)
