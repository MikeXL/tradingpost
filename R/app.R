library(quantmod)
library(PerformanceAnalytics)
# SMA trading strategy
strat_sma <- function(price){
  sma1 <- SMA(price, 10)
  sma2 <- SMA(price, 30)
  signal <- lag(ifelse(sma1 > sma2, 1, 0))
  signal
}

strat_ema <- function(price){
  ema1 <- EMA(price, 12)
  ema2 <- EMA(price, 26)
  signal <- lag(ifelse(ema1 > ema2, 1, 0))
  signal
}

# MACD trading strategy
strat_macd <- function(x){
  macd   <- MACD(x, 12, 26, 9, EMA, F)
  signal <- lag(ifelse(macd$macd > macd$signal, 1, 0))
  signal
}

# RSI, CCI trading strategy
strat_rsi_cci <- function(prices){
  RSI30  <- RSI(prices, 30)
  RSI50  <- RSI(prices, 50)
  CCI6   <- CCI(prices, 6)
  CCI100 <- CCI(prices, 100)
  
  signal <- lag(ifelse(RSI30 > RSI50 & CCI6 > CCI100, 1, 0))
  signal
}

# DEMA trading strategy
strat_dema <- function(prices) {
  dema12 <- DEMA(prices, 12)
  dema14 <- DEMA(prices, 14)
  
  signal <- lag(ifelse(dema12 > dema14, 1, 0))
  signal
}

# DVI trading strategy, surprisingly well
strat_dvi <- function(x) {
  dvi <- DVI(x)
  sig <- Lag(ifelse(dvi$dvi < .5, 1, 0))
  sig
}

# neural net trading strategy
strat_nnet <- function(prices) {
  # nothing here yet
}

# Define the UI
ui <- bootstrapPage(
  textInput("stock", "Stock Symbol: ", "BABA"),
  textInput("dt_start", "Starting Period:", "2019-01-01"),
  textInput("dt_end", "End Period: ", "2019-12-25"),
  plotOutput('plot', width=800, height=400),
  plotOutput('plot_perf', width=800, height=400)
)

x = NA
# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    sym = input$stock
    getSymbols(sym)
    x = get(sym)
    period = paste(input$dt_start, input$dt_end, sep="/")
    chartSeries(x[period], TA="addEMA(12);addEMA(26);addMACD();addRSI();addSMI()", 
                theme="white", main=sym
                )

  })
  output$plot_perf <- renderPlot({
    sym = input$stock
    getSymbols(sym)
    x = get(sym)
    data = Cl(x)
    backtest_period = paste(input$dt_start, input$dt_end, sep="/")
    
    returns_macd     <- ROC(data) * strat_macd(data)
    returns_rsi_cci  <- ROC(data) * strat_rsi_cci(data)
    returns_dvi      <- ROC(data) * strat_dvi(data)
    returns_sma      <- ROC(data) * strat_sma(data)
    returns_ema      <- ROC(data) * strat_ema(data)
    returns_dema     <- ROC(data) * strat_dema(data)
    returns_base     <- dailyReturn(data)
    
    rets_macd        <- returns_macd    [backtest_period]
    rets_rsi_cci     <- returns_rsi_cci [backtest_period]
    rets_dvi         <- returns_dvi     [backtest_period]
    rets_sma         <- returns_sma     [backtest_period]
    rets_ema         <- returns_ema     [backtest_period]
    rets_dema        <- returns_dema    [backtest_period]
    rets_base        <- returns_base    [backtest_period]
    
    
    names(rets_macd)     <- "MACD"
    names(rets_rsi_cci)  <- "RSI CCI"
    names(rets_dvi)      <- "DVI"
    names(rets_sma)      <- "SMA"
    names(rets_ema)      <- "EMA"
    names(rets_dema)     <- "DEMA"
    names(rets_base)     <- "Buy and Hold"
    
    charts.PerformanceSummary(
      cbind( rets_macd, rets_base, rets_dvi, rets_sma, rets_ema, rets_rsi_cci, rets_dema), 
      main="Performance of Strategies"
    )
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)



