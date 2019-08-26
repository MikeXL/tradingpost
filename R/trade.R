library(quantmod)
library(PerformanceAnalytics)

getSymbols(c("SPY", "MSFT", "SBUX"))

backtest_period  <- '2018-01-01/2018-12-31'

# chart MSFT stock in full 
#  and back test period
chartSeries(MSFT, TA='addMACD();addRSI();addCCI();addDEMA();addROC()')
chartSeries(MSFT[backtest_period], TA='addMACD();addRSI();addCCI();addDEMA();addROC()')

# trading strategies
# return buy and sell signal 1, -1
#
strat_macd <- function(x){
  macd   <- MACD(Cl(x), 12, 26, 9, SMA, F)
  signal <- lag(ifelse(macd$macd < macd$signal, -1, 1))
  signal
}

strat_rsi_cci <- function(prices){
  RSI30  <- RSI(prices, 30)
  RSI50  <- RSI(prices, 50)
  CCI6   <- CCI(prices, 6)
  CCI100 <- CCI(prices, 100)
  
  signal <- lag(ifelse(RSI30 > RSI50 & CCI6 > CCI100, 1, -1))
  signal
}

strat_dema <- function(prices) {
  dema12 <- DEMA(prices, 12)
  dema14 <- DEMA(prices, 14)
  
  signal <- lag(ifelse(dema12 > dema14, 1, -1))
  signal
}

strat_dvi <- function(x) {
  dvi <- DVI(x)
  sig <- Lag(ifelse(dvi$dvi < .5, 1, -1))
  sig
}

# backtest with MSFT 2018 stock, closing price

data = Cl(MSFT)
returns_macd     <- ROC(data) * strat_macd(data)
returns_rsi_cci  <- ROC(data) * strat_rsi_cci(data)
returns_dvi      <- ROC(data) * strat_dvi(data)
returns_dema     <- ROC(data) * strat_dema(data)
returns_base     <- dailyReturn(data)

rets_macd        <- returns_macd    [backtest_period]
rets_rsi_cci     <- returns_rsi_cci [backtest_period]
rets_dvi         <- returns_dvi     [backtest_period]
rets_dema        <- returns_dema    [backtest_period]
rets_base        <- returns_base    [backtest_period]


names(rets_macd)     <- "MACD"
names(rets_rsi_cci)  <- "RSI CCI"
names(rets_dvi)      <- "DVI"
names(rets_dema)     <- "DEMA"
names(rets_base)     <- "daily return"

charts.PerformanceSummary(
    cbind( rets_macd, rets_base, rets_dvi, rets_rsi_cci, rets_dema), 
    main="Performance of Strategies"
)

