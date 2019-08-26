cash = 10000
holdings {symbol, num_shares, purchase_price, current_price}

holding_history

getSymbols(c("SPY", "MSFT", "SBUX", "^NSEI"))

sym <- SPY[,4]    # closing price
sym <- MSFT

strat_macd <- function(x){
  macd <- MACD(Cl(x), 12, 26, 9, SMA, F)
  signal <- lag(ifelse(macd$macd < macd$signal, -1, 1))
  signal
}

strat_rsi_cci <- function(prices){
  RSI30 <- RSI(prices, 30)
  RSI50 <- RSI(prices, 50)
  CCI6 <- CCI(prices, 6)
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


data = Cl(MSFT)
returns_macd <- ROC(data) * strat_macd(data)
returns_rsi_cci  <- ROC(data) * strat_rsi_cci(data)
returns_dvi  <- ROC(data) * strat_dvi(data)
returns_dema <- ROC(data) * strat_dema(data)
returns_base <- dailyReturn(data)

backtest_period = '2018-01-01/2018-12-31'
rets_macd <- returns_macd[backtest_period]
rets_rsi_cci  <- returns_rsi_cci [backtest_period]
rets_dvi  <- returns_dvi [backtest_period]
rets_dema <- returns_dema[backtest_period]
rets_base <- returns_base[backtest_period]


names(rets_macd) <- "MACD"
names(rets_rsi_cci)  <- "RSI CCI"
names(rets_dvi)  <- "DVI"
names(rets_dema) <- "DEMA"
names(rets_base)  <- "daily return"
charts.PerformanceSummary(cbind( rets_macd, rets_base, rets_dvi, rets_rsi_cci, rets_dema))







# backtest 2012-01-01 to 2018-12-31
sig <- signal['2018-01-01/2018-12-31']
sig$lag2 <- lag(sig$Lag.1, 1)


# strategy I, buy then hold til next sell signal
cash = 10000
cash_hist = matrix(, nrow=nrow(sig), ncol=1)
invest = 1000
holdings = 0

for(i in 2:nrow(sig)){
  price = as.numeric(sym[i, ])
  if(sig[i, ]$Lag.1 == sig[i, ]$lag2){
    # hold
    # do nothing
    cat("\n")
  }
  if(sig[i, ]$lag2== -1 & sig[i, ]$Lag.1==1){
    # buy
    holdings = holdings + invest/price
    cash = cash - invest
    cat("+", i, holdings, cash, "\n")
  }
  if(sig[i, ]$lag2== 1 & sig[i, ]$Lag.1 == -1){
    # sell
    cash = cash + holdings * price
    cash_hist[i] = cash
    holdings = 0
    cat("-", i, holdings, cash, "\n")
  }
}
plot(cash_hist)

# strategy II, buy, continue to buy, and sell
cash = 100000
cash_hist = matrix(, nrow=nrow(sig)+1, ncol=3)
invest = 1000
holdings = 0

for(i in 1:nrow(sig)){
  price = as.numeric(sym2[i, ]$MSFT.Close)
  
  if(sig[i, ]$Lag.1==1){
    # buy
    holdings = holdings + invest/price
    cash = cash - invest

    cat("+", i, holdings, cash, "\n")
  }
  if(sig[i, ]$Lag.1 == -1){
    # sell
    cash = cash + holdings * price
    holdings = 0
    cat("-", i, holdings, cash, "\n")
  }
  cash_hist[i, 1] = cash
  cash_hist[i, 2] = price
  cash_hist[i, 3] = holdings
}
par(mfrow=c(2,2))
plot(cash_hist[,1], type="l")
plot(cash_hist[,2], type="l")
plot(cash_hist[,3], type="l")

