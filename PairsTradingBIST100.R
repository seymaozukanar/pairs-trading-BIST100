### PAIRS TRADING ###

#Load libraries
library(dplyr)
library(Quandl)
library(quantmod)
library(quantstrat)
library(urca)
library(xts)
library(zoo)

# PAIRS IDENTIFICATION

# Define stock symbols (BIST100 + KRDMA + KRDMB)
BIST_100 <- c(
  "AEFES.IS", "AGROT.IS", "AHGAZ.IS", "AKBNK.IS", "AKCNS.IS", "AKFGY.IS", "AKFYE.IS", "AKSA.IS", "AKSEN.IS", "ALARK.IS",
  "ALBRK.IS", "ALFAS.IS", "ANSGR.IS", "ARCLK.IS", "ASELS.IS", "ASTOR.IS", "BERA.IS", "BFREN.IS", "BIENY.IS", "BIMAS.IS",
  "BIOEN.IS", "BOBET.IS", "BRSAN.IS", "BRYAT.IS", "BTCIM.IS", "CANTE.IS", "CCOLA.IS", "CIMSA.IS", "CWENE.IS", "DOAS.IS",
  "DOHOL.IS", "ECILC.IS", "ECZYT.IS", "EGEEN.IS", "EKGYO.IS", "ENERY.IS", "ENJSA.IS", "ENKAI.IS", "EREGL.IS", "EUPWR.IS",
  "EUREN.IS", "FROTO.IS", "GARAN.IS", "GESAN.IS", "GUBRF.IS", "GWIND.IS", "HALKB.IS", "HEKTS.IS", "IPEKE.IS", "ISCTR.IS",
  "ISGYO.IS", "ISMEN.IS", "IZENR.IS", "KAYSE.IS", "KCAER.IS", "KCHOL.IS", "KLSER.IS", "KONTR.IS", "KONYA.IS", "KOZAA.IS",
  "KOZAL.IS", "KRDMD.IS", "MAVI.IS", "MGROS.IS", "MIATK.IS", "ODAS.IS", "OTKAR.IS", "OYAKC.IS", "PETKM.IS", "PGSUS.IS",
  "QUAGR.IS", "REEDR.IS", "SAHOL.IS", "SASA.IS", "SAYAS.IS", "SDTTR.IS", "SISE.IS", "SKBNK.IS", "SMRTG.IS", "SOKM.IS",
  "TABGD.IS", "TAVHL.IS", "TCELL.IS", "THYAO.IS", "TKFEN.IS", "TOASO.IS", "TSKB.IS", "TTKOM.IS", "TTRAK.IS", "TUKAS.IS",
  "TUPRS.IS", "TURSG.IS", "ULKER.IS", "VAKBN.IS", "VESBE.IS", "VESTL.IS", "YEOTK.IS", "YKBNK.IS", "YYLGD.IS", "ZOREN.IS",
  "KRDMA.IS", "KRDMB.IS"
)

# Retrieve OHLC price data of stocks
stock_data <- list()
for (symbol in BIST_100) {
  stock_data[[symbol]] <- getSymbols(symbol, src = "yahoo", from = "1900-01-01")
}

# Fill NA values with LOCF
for (symbol in BIST_100) {
  assign(symbol, na.locf(get(symbol)))
}

# Get start dates
start_dates <- c()
for (symbol in BIST_100) {
  start_date <- start(get(symbol))
  start_dates <- c(start_dates, start_date)
}

min_start_date <- min(as.Date(start_dates)) # Find the earliest start date among all stocks

# Find stocks with the earliest start dates
stocks <- c()
for (symbol in BIST_100) {
  start_date <- start(get(symbol))
  if(start_date <= min_start_date){
    stocks <- c(stocks, symbol)
  }
}

print(stocks)

# Normalize the close prices of the first five years
normalized_prices <- lapply(stocks, function(stock_name) {
  stock <- get(stock_name)
  stock <- stock[1:(5*260), ]
  normalized_price <- stock[,4]/as.numeric(stock[1,4])
  return(normalized_price)
})

# Calculate SSD values
pair_data <- data.frame(Stock_1 = character(), Stock_2 = character(), SSD = numeric())
for (i in 1:length(stocks)) {
  for (j in 1:length(stocks)) {
    if (i != j) {
      ssd <- sum((normalized_prices[[i]] - normalized_prices[[j]])^2)
      pair_data <- rbind(pair_data, data.frame(Stock_1 = stocks[i], Stock_2 = stocks[j], SSD = ssd))
    }
  }
}

# Find the lowest SSD pairs for each stock
lowest_ssd_pairs <- pair_data %>%
  group_by(Stock_1) %>%
  slice(which.min(SSD))

# Get all stock combinations
combinations <- combn(stocks, 2)
pairs <- split(combinations, col(combinations))

# Define a function for cointegration test
test_cointegration <- function(stock1, stock2) {
  stock1 <- get(stock1)
  stock2 <- get(stock2)
  log_prices1 <- log(stock1[,4])
  log_prices2 <- log(stock2[,4])
  
  regression <- lm(log_prices1 ~ log_prices2)
  residuals <- regression$residuals
  adf_test <- adf.test(residuals)
  p_value <- adf_test$p.value
  
  return(p_value)
}

# Perform cointegration test for each combination 
results <- lapply(pairs, function(pair) {
  stock1 <- pair[1]
  stock2 <- pair[2]
  p_value <- test_cointegration(stock1, stock2)
  data.frame(stock1 = stock1, stock2 = stock2, p_value = p_value)
})

# Filter significant pairs (p-value is lower than or equal to 0.05)
significant_pairs <- Filter(function(x) x$p_value <= 0.05, results)

# DIVERGENCE DETECTION

checkDistanceApp <- function(stockA, stockB){
  formationPeriod <- 85
  
  #Fetch stock data
  getSymbols(stockA, src="yahoo", from=as.Date("1900-01-01"))
  A <- get(stockA)
  getSymbols(stockB, src="yahoo", from=as.Date("1900-01-01"))
  B <- get(stockB)
  
  closingPricesA <- na.omit(tail(Cl(A), 3*260))
  closingPricesB <- na.omit(tail(Cl(B), 3*260))
  
  indexedPriceA <- closingPricesA[
                   (length(closingPricesA) -
                   formationPeriod + 1)
                   ][[1]]
  indexedPriceB <- closingPricesB[
                   (length(closingPricesB) -
                   formationPeriod + 1)
                   ][[1]]
  
  normalizedPricesA <- closingPricesA / indexedPriceA
  normalizedPricesB <- closingPricesB / indexedPriceB
  normalizedDiff <- normalizedPricesA - normalizedPricesB
  
  # Check the following for both A and B:
  # normalizedPricesA[length(closingPricesA) - formationPeriod + 1] == 1
  
  mean <- mean(normalizedDiff)
  std <- sd(normalizedDiff)
  
  # Calculate z-score of latest normalized difference
  zScore <- (tail(normalizedDiff, 1) - mean) / std
  return(zScore)
}


checkCointegrationApp <- function(stockA, stockB){
  #Fetch stock data
  getSymbols(stockA, src="yahoo", from=as.Date("2021-01-01"))
  A <- get(stockA)
  getSymbols(stockB, src="yahoo", from=as.Date("2021-01-01"))
  B <- get(stockB)
  
  closingPrices<-na.omit(tail(Cl(merge(A,B)), 3*260))
  
  # Co-integration equation
  fit <- lm(log(closingPrices[,1])~log(closingPrices[,2]))
  coeffs <- summary(fit)$coefficients
  r_2 <- summary(fit)$r.squared
  
  # Co-integration Residual
  spread <- residuals(fit)
  
  # Calculate z-scores
  zScores <- (spread-mean(spread))/sd(spread)
  
  # Check for unit root
  # Perform Augmented Dickey-Fuller test
  testResults <- UR_Test_Res<-summary(ur.df(spread))
  testCoeffs <- UR_Test_Res@testreg$coefficients
  
  testStats <- UR_Test_Res@teststat
  criticalValues <- UR_Test_Res@cval
  
  return(zScores)
}

#z1 <- checkCointegrationApp("THY", "PGSUS.IS")
#z2 <- checkDistanceApp("THY", "PGSUS.IS")

#z3 <- checkCointegrationApp("AKBNK.IS", "GARAN.IS")
#z4 <- checkDistanceApp("AKBNK.IS", "GARAN.IS")


if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 
ls(all=T) #.blotter and .strategy environments added
class(.blotter)

# Define instruments
currency("USD")
stock("BIST",currency="USD",multiplier=1)

# Get data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Data<-read.csv(file = "XU100.csv",sep = ";")
Data<-zoo(Data[,-1],as.Date(as.character(Data[,1]),format="%Y%m%d"))
names(Data)<-c("Open","High","Low","Close","Volume")
plot(Data)

.from='2005-08-01'
.to='2016-05-25'

BIST<-xts(coredata(Data),
          as.POSIXct(time(Data)))#Must be POSIXct
BIST<-BIST[paste0(.from,"/",.to)]

# Define strategy component names
strategy.st = 'GoldenCross'
portfolio.st = 'TrendFollowing'
account.st = 'ABCInvest'

# If you removed all objects from the global environment,
# then you may need to recreate .blotter and .strategy environments
#.blotter<-new.env()
#.strategy<-new.env()

# If you previously run the same strategy: 
# You should first remove old strategy/order book/account/portfolio objects 
rm.strat(strategy.st)
rm.strat(portfolio.st)
rm.strat(account.st)
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 


# Initialize portfolio&account in .blotter, 
# and orderbook&strategy in .strategy environments
initDate<-as.character(as.Date(.from)-1) # One day before data starts
initEq<-30000

initPortf(portfolio.st, 
          symbols='BIST', 
          initDate=initDate, 
          currency='USD')
initAcct(account.st, 
         portfolios=portfolio.st, 
         initDate=initDate, 
         currency='USD',
         initEq=initEq)
initOrders(portfolio.st, 
           initDate=initDate)
strategy(strategy.st, 
         store=TRUE)



# See what's inside the environments
ls(envir=FinancialInstrument:::.instrument)
temp<-get("BIST",envir = FinancialInstrument:::.instrument)
temp<-get("USD",envir = FinancialInstrument:::.instrument)

ls(all=T) #.blotter and .strategy environments are inside Global Env

ls(all=T,envir=.blotter)
temp<-get("account.ABCInvest",envir = .blotter)
temp<-get("portfolio.TrendFollowing",envir = .blotter)

ls(all=T,envir=.strategy)
temp<-get("order_book.TrendFollowing",envir = .strategy)
temp<-get("GoldenCross",envir = .strategy)

class(temp) # Analyze the object class
str(temp) # And its structure 
summary(temp) # Use this especially for strategy object




# Add indicators
.fast = 10
.slow = 20

add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)

summary(get("GoldenCross",envir = .strategy))

# Add signals

add.signal(strategy.st, 
           name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)

summary(get("GoldenCross",envir = .strategy))

# Add rules (i.e. when to send orders)
.orderqty = 1
.threshold = 0.005
.txnfees = 0		# round-trip fee

add.rule(strategy.st, 
         name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', 
                        prefer='High', 
                        threshold=.threshold,
                        tmult=TRUE,
                        orderqty=+.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', 
                        sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees, #Only on exits
                        replace=TRUE #Replace any pending open orders
         ),
         type='exit',
         label='Exit2SHORT'
)


add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', 
                        sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', 
                        prefer='Low', 
                        threshold=-.threshold,
                        tmult=TRUE,
                        orderqty=-.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , 
                        sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,#Only on exits
                        replace=TRUE #Replace any pending open orders
         ),
         type='exit',
         label='Exit2LONG'
)


summary(get("GoldenCross",envir = .strategy))


# Apply strategy
applyStrategy(strategy.st, portfolio.st)

# Update portfolio & account
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

# Analyze performance
chart.Posn(portfolio.st, "BIST")

################################
###   IN-SAMPLE OPTIMIZATION
################################


# Define parameter space 
.FastSMA = c(1,3,5,10,15,20,50)
.SlowSMA = c(10,20,50,100,150,200)

.FastSMA = c(1,3,5,10)
.SlowSMA = c(10,20,50)

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nFast',
                 variable = list(n = .FastSMA),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'SMA',
                 component.type = 'indicator',
                 component.label = 'nSlow',
                 variable = list(n = .SlowSMA),
                 label = 'nSLOW'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'SMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'SMA'
)

summary(get("GoldenCross",envir = .strategy))

# Apply parameter optimization
library(doParallel)
detectCores()
registerDoParallel(cores=8) # Parallel computing

# Use nsamples if you want random samples from the parameter space
results <- apply.paramset(strategy.st, 
                          paramset.label='SMA', 
                          portfolio.st=portfolio.st, 
                          account.st=account.st, 
                          verbose=TRUE)
stopImplicitCluster()

# Analyze results
class(results) # A long list object containing results
names(results) # "tradeStats" contains summaries

stats <- results$tradeStats
View(stats)
names(stats)

# Function for plotting
require(akima)
require(plot3D)

Heat.Map<-function(x,y,z,title){
  s=interp(x,y,z)
  image2D(s,main=title)
}


# Plot results
par(mfrow=c(3,2),mar=c(2,2,2,2)) # 3x2 plots on same page
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Net.Trading.PL"],"Net.Trading.PL")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Percent.Positive"],"Percent.Positive")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Profit.Factor"],"Profit.Factor")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Ann.Sharpe"],"Ann.Sharpe")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Max.Drawdown"],"Max.Drawdown")
Heat.Map(stats[,"nFAST"],stats[,"nSLOW"],stats[,"Profit.To.Max.Draw"],"Profit.To.Max.Draw")
par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1))

################################
###   WALK FORWARD ANALYSIS
################################

# IMPORTANT: If you previously run in sample optimization part,
# you should start from a fresh new quantstrat session
# before running WFA


# Perform WFA
# Optimize over last 36 months
# Trade with optimized params during next 12 months
# Only search for 10 param combos
# Select the param combo with the highest profit/max drawdown
# Rolling WFA, not anchored

WFA<-walk.forward(
  strategy.st=strategy.st, 
  portfolio.st=portfolio.st, 
  account.st=account.st,
  paramset.label='SMA', # Use this paramset
  period='months', 
  k.training=36, # Optimize over last 36 months
  k.testing=12, # Trade with optimized params during next 12 months
  nsamples=10, # Only search for 10 param combos
  obj.func=function(x){which(x == max(x,na.rm=T))},#Obj fnc 
  obj.args=list(x=quote(tradeStats.list$Profit.To.Max.Draw)), #Obj fnc args
  audit.prefix='wfa', # Will be used in creating RData filenames
  anchored=FALSE,# Rolling WFA, not anchored
  savewf=T,saveenv =F) # Convert to TRUE for further audit 

# WFA Settings
WFA$wf.subsets # Test and training periods

# Check the files generated in WFA
# getwd() --> location of files
fls<-list.files(pattern="^wfa.*\\.RData$")

audit.file<-fls[1]
load(audit.file)
ls(all=TRUE) # note ".audit" envir
ls(.audit)
View(.audit$tradeStats)

fls_res<-"wfa.BIST.20120801T030000.20150731T030000.Results.RData"
env_res<-new.env()
load(fls_res,envir = env_res)
ls(env_res)
View(t(env_res$results$tradeStats)) # OOS trade


# Analyze training set results
chart.forward.training(fls[1])
chart.forward.training(fls[2])
chart.forward.training(fls[3])

# Analyze test set results
WFA$testing.parameters # Selected optimal parameters used in test periods
View(t(WFA$tradeStats)) # Test portfolio trading statistics 
View(WFA$blotter$portfolio.test.TrendFollowing$summary) # test portfolio blotter

chart.forward(fls_res)



