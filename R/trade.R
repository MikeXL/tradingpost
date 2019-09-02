library(quantmod)
library(PerformanceAnalytics)

getSymbols(c("SPY", "MSFT", "SBUX", "WMT", "COKE"))

# backtest_period  <- '2018-01-01/2018-12-31'
backtest_period = '2015-01-01/2016-12-31'
  
# chart MSFT stock in full 
#  and back test period
chartSeries(MSFT, TA='addMACD();addRSI();addCCI();addDEMA();addROC()')
chartSeries(MSFT[backtest_period], TA='addMACD();addRSI();addCCI();addDEMA();addROC()')

# trading strategies
# return buy and sell signal 1, -1
#
# SMA trading strategy
strat_sma <- function(price){
  sma5 <- SMA(price, 10)
  sma20 <- SMA(price, 30)
  signal <- lag(ifelse(sma5 > sma20, 1, 0))
  signal
}

# MACD trading strategy
strat_macd <- function(x){
  macd   <- MACD(Cl(x), 12, 26, 9, SMA, F)
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

# backtest with MSFT 2018 stock, closing price
# data = Cl(SPY)

# simple portfolio = 50% MSFT + 40% Starbucks + 10% S&P index
# data = Cl(MSFT)*.5 + Cl(SBUX)*.4 + Cl(SPY)*.1
data = Cl(SPY)

returns_macd     <- ROC(data) * strat_macd(data)
returns_rsi_cci  <- ROC(data) * strat_rsi_cci(data)
returns_dvi      <- ROC(data) * strat_dvi(data)
returns_sma      <- ROC(data) * strat_sma(data)
returns_dema     <- ROC(data) * strat_dema(data)
returns_base     <- dailyReturn(data)

rets_macd        <- returns_macd    [backtest_period]
rets_rsi_cci     <- returns_rsi_cci [backtest_period]
rets_dvi         <- returns_dvi     [backtest_period]
rets_sma         <- returns_sma     [backtest_period]
rets_dema        <- returns_dema    [backtest_period]
rets_base        <- returns_base    [backtest_period]


names(rets_macd)     <- "MACD"
names(rets_rsi_cci)  <- "RSI CCI"
names(rets_dvi)      <- "DVI"
names(rets_sma)      <- "SMA"
names(rets_dema)     <- "DEMA"
names(rets_base)     <- "Buy and Hold"

par(mfrow=c(2,1))
chartSeries(data[backtest_period], TA="addMACD();addRSI()", title="portfolio = 50% MSFT + 40% Starbucks + 10% S&P index")

charts.PerformanceSummary(
    cbind( rets_macd, rets_base, rets_dvi, rets_sma, rets_rsi_cci, rets_dema), 
    main="Performance of Strategies"
)

# What we learnt so far
# No single indicator works for all stocks, though only tested on MSFT, Starbucks and S&P index
# Indicator like SMA or DVI may work on one stock, but no the other
# might not work on different period either ? 
# Future works, monte carlo backtesting? CV ?

