library(quantmod)
library(Quandl)
library(xts)
library(dplyr)
library(hydroTSM)
library(PerformanceAnalytics)
library(stargazer)
library(car)
library(het.test)
library(estimatr)
library(car)
library(tseries)
library(ggplot2)
library(MASS)
library(longmemo)
library(urca)
library(vars)
#install.packages("longmemo")
#install.packages("MASS")
#install.packages("estimatr")
#install.packages("Rcpp")
#install.packages("rlang")
options(scipen = 3)
setwd("C:/Users/Denis Korsak/Desktop/Education/B.A. Economics YorkU/Econometrics 4260/Assignments/Assignment 3")
#Data Download
Quandl.api_key(api_key = "VvTvysNw1eL-xYjz-pai")

getSymbols("^IXIC", src = "yahoo", auto.assign = FALSE) %>% window(, start = "2015-01-02", end = "2017-12-01") %>% as.xts -> nasdaq
Quandl("BOC/V39065", type = "xts") %>% window(,start = "2015-01-02", end = "2017-12-01") %>% as.xts -> tbills
getSymbols("AAPL", src = "yahoo", auto.assign = FALSE) %>% window(,start = "2015-01-02", end = "2017-12-01") %>% as.xts -> apple
mv <- read.csv("DD.csv")
mv <- mv[,-2]
mv <- mv[1:502,]
mv$Date <- as.Date(mv$Date, format = "%d/%m/%Y")
mv <- as.data.frame(mv)
mv <- as.xts(mv, order.by = mv$Date)
mv <- mv[,-1]
mv <- mv["2016-06-10/2017-12-01"]

#Data Preparation
annualized <- function(x){(((1 + x)^12) - 1)}
apple$AAPL.Close %>% Delt(,k=1) %>% annualized -> ret.apple
nasdaq$IXIC.Close %>% Delt(,k=1) %>% annualized -> ret.nasdaq
marketcap <- apple$AAPL.Volume*apple$AAPL.Close 
tbills/100 -> tbills.decf
merge(ret.apple,ret.nasdaq,tbills.decf, marketcap) -> data
data$`Market Cap` <- scale(data$`Market Cap`)
rm(apple,ret.apple,ret.nasdaq,nasdaq,tbills.decf,tbills)
data <- data[-1,]
data$apple_risk.p <- data$Delt.1.arithmetic - data$tbills.decf
data$market_risk.p <- data$Delt.1.arithmetic.1 - data$tbills.decf
data <- na.omit(data)
colnames(data) <- c("Apple","Nasdaq","Tbills","Market Cap", "Apple Risk Premium","Market Risk Premium")

#CAPM & Fama-French
capm <- lm(`Apple Risk Premium` ~ `Market Risk Premium`, data = data)
data_s <- data["2016-06-10/2017-12-01"]
data_s <- cbind(data_s, mv) %>% na.omit
colnames(data_s) <- c("Apple","Nasdaq","Tbills","Market Cap", "Apple Risk Premium","Market Risk Premium","P/B")
data_s$`Market Cap` <- scale(data_s$`Market Cap`)
data_s$`P/B` <- Delt(data_s$`P/B`, k = 1)
rm(mv,marketcap)
data_s <- as.data.frame(data_s)
data_s <- data_s[-1,]
ff3 <- lm(data_s$`Apple Risk Premium`~data_s$`Market Risk Premium` + data_s$`Market Cap`+data_s$`P/B`)
stargazer(capm, ff3, type ="text", title = "CAPM & Fama-French", omit.stat = c("LL","AIC", "BIC"), dep.var.labels = c("Apple Risk Premium", "Apple Risk Premium"), covariate.labels = "Market Risk Premium")

#Heteroscedasticity, autocorrelation, multicollinearity, ADF,PP,KPSS
#1.Breusch - Pagan Test
bptest(capm)
bptest(ff3)

#Correction
capm <- lm_robust(`Apple Risk Premium` ~ `Market Risk Premium`, data = data)
#Could also standardize everything.

#2.Derbin-Watson Test
dwtest(capm)
dwtest(ff3)

#both models would pass the test on 10 % interval. Could add lags to model to fix the issue. 

#3. Multicollinearity
vif(ff3)
#No need for CAPM since only one variable

#4.Unit Root Tests: Dickey-Fuller Test
adf.test(data$`Apple Risk Premium`, k = 1)
adf.test(data$`Market Risk Premium`, k = 1)
adf.test(data$`Market Cap`, k = 1)
adf.test(data_s$`P/B`, k = 1)

#Unit Root Tests : PP test
pp.test(data$`Apple Risk Premium`)
pp.test(data$`Market Risk Premium`)
pp.test(data$`Market Cap`)
pp.test(data_s$`P/B`)

#Unit Root Tests 
kpss.test(data$`Apple Risk Premium`)
kpss.test(data$`Market Risk Premium`)
kpss.test(data$`Market Cap`)
kpss.test(data_s$`P/B`)

#Plotting Market and Stock Prices

#1. Getting the data
getSymbols("^OSPTX", src = "yahoo", auto.assign = FALSE) %>% window(, start = "2015-01-02", end = "2017-12-01") %>% as.xts -> nasdaq
getSymbols("AAPL", src = "yahoo", auto.assign = FALSE) %>% window(,start = "2015-01-02", end = "2017-12-01") %>% as.xts -> apple

nasdaq <- Cl(nasdaq)
apple <- Cl(apple)
#2. Plotting Stock and Index
par(mfrow = c (2,1))
plot.zoo(apple, type = "l", main = "", xlab = "2015/2017",
         grid.ticks.on = "months", grid.ticks.lty = 0, ylab = "Stock Price")
axis(side = 4, at = pretty(apple))
title("Apple Stock Price", line = -2)
plot.zoo(nasdaq, type = "l", main = "", xlab = "2015/2017",
         grid.ticks.on = "months", grid.ticks.lty = 0, ylab = "Daily Value")
title("Nasdaq Trade Volume", line = -2)
axis(side = 4, at = pretty(nasdaq))

#Histogram
par(mfrow = c(2,1))
hist(Cl(apple), nclass = 25, probability = F, main = "Distribution of Apple Price")
hist(Cl(nasdaq), nclass = 25, probability = F, main = "Distribution of Nasdaq Performance")

#3. Plotting the Returns
par(mfrow = c (2,1))
plot.zoo(Cl(diff(apple)), type = "l", main = "Apple Stock Returns", xlab = "2015/2017",
         grid.ticks.on = "months", grid.ticks.lty = 0, ylab = "Volatility of Returns ($)")
plot.zoo(Cl(diff(nasdaq)), type = "l", main = "Nasdaq Index", xlab = "2015/2017",
         grid.ticks.on = "months", grid.ticks.lty = 0, ylab = "Volatility Daily ($)")

#Histogram of the Returns
par(mfrow = c (2,2))
hist(data$Apple, nclass = 100, probability = F, main = "", ylim = c(0,50))
title("Distribution of Apple Returns", line = -2)
plot(density(data$Apple), ylim = c(0,4), main = "")
title("Density Fucntion of Apple Distributions", line = -2)
qqnorm(data$Nasdaq, main = "")
title("Normal QQ plot", line = -2)
qqline(data$Nasdaq)

par(mfrow = c (2,2))
hist(data$Nasdaq, nclass = 100, probability = F, main = "", ylim = c(0,60))
title("Distribution of Nasdaq Returns", line = -2)
plot(density(data$Nasdaq), ylim = c(0,6), main = "")
title("Density Fucntion of Nasdaq Distributions", line = -2)
qqnorm(data$Nasdaq, main = "")
title("Normal QQ plot", line = -2)
qqline(data$Nasdaq)


#3.Performing Jarque-Berra test
jarque.bera.test(data$Apple)
jarque.bera.test(data$Nasdaq)


#4.Unit Root Tests: Dickey-Fuller Test
apple <- as.ts(apple)
nasdaq <- as.ts(nasdaq)
adf.test(data$Apple)
adf.test(apple) #Non-Stationary
adf.test(data$Nasdaq)
adf.test(nasdaq) # Non - Stationary

#Unit Root Tests : PP test
pp.test(data$Apple)
pp.test(apple) #Non-Stationary
pp.test(data$Nasdaq)
pp.test(nasdaq) # Non - Stationary

#Unit Root Tests 
kpss.test(data$Apple)
kpss.test(apple, null = "T") #Non-Stationary
kpss.test(data$Nasdaq)
kpss.test(nasdaq, null = "T") # Non - Stationary


#Tests for Random walk of the data
pp.test(apple) #Non-Stationary
pp.test(nasdaq) # Non - Stationary 
#Pp.test suggest random walk in both cases


acf(apple, 100)
acf(nasdaq,100)

#Testing for long range dependance
d <- WhittleEst(nasdaq)
plot(d)

#Result - overall: random walk

rm(d,apple, annualized, capm, data, data_s, ff3, nasdaq)

#6. Johansen cointegration tests
getSymbols("^GSPTSE", src = "yahoo", auto.assign = FALSE) %>% window(, start = "2015-01-02", end = "2017-12-01") %>% as.xts -> tsxcad
getSymbols("^DJI", src = "yahoo", auto.assign = FALSE) %>% window(, start = "2015-01-02", end = "2017-12-01") %>% as.xts -> dowusa
getSymbols("^FTSE", src = "yahoo", auto.assign = FALSE) %>% window(, start = "2015-01-02", end = "2017-12-01") %>% as.xts -> ftse
data <- cbind(Cl(dowusa), Cl(tsxcad)) %>% na.omit
jotest <- ca.jo(data, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

#At all levels we have presense of cointegration
#First, run the VAR

varmodel <- VAR(y = data, p = 1, type = "trend")
class(varmodel) = "varest"

#7. Granger Causality
data <- cbind(data, Cl(ftse)) %>% na.omit
colnames(data) <- c("Dow","TSXcad","FTSE")
#1.
grangertest(data$Dow, data$TSXcad, order = 2) #Evidence of no granger causality

#2.
grangertest(data$Dow, data$FTSE, order = 2) #Evidence of granger causality

#3. 
grangertest(data$FTSE, data$TSXcad, order = 2) #Evidence of no granger causality

#8. Variance decomposition
fevd(x = varmodel, n.ahead = 5)

#9. Impulse Response Functions
impulse_resp <- irf(varmodel, impulse = NULL, response = NULL, n.ahead = 3,runs = 100)

rm(data,dowusa,ftse,impulse_resp,jotest,rbc,tsxcad,varmodel)
#Assignment 3 : VAR

#1. Getting the data
annualized <- function(x){(((1 + x)^12) - 1)}

getSymbols("RY.TO", src = "yahoo", auto.assign = FALSE) %>% window(,start = "2012-01-02", end = "2017-12-01") %>% to.weekly %>% Cl %>% as.xts -> rbc
Delt(rbc, k= 1) %>% annualized -> rbcw.ret
getSymbols("^GSPTSE", src = "yahoo", auto.assign = FALSE) %>% window(,start = "2012-01-02", end = "2017-12-01") %>% to.weekly %>% Cl %>% as.xts -> tsx
Delt(tsx, k= 1) %>% annualized -> tsxw.ret

Quandl("BOC/V39065", type = "xts") %>% window(,start = "2012-01-02", end = "2017-12-01")  -> tbills
tbills <- to.weekly(tbills)
tbills <- Cl(tbills)


getSymbols("CADUSD=X", src = "yahoo", auto.assign = FALSE) %>% window(,start = "2012-01-02", end = "2017-12-01") %>% to.weekly %>% Cl %>% as.xts -> cad_us
Delt(cad_us, k= 1) %>% annualized -> cad_us


rm(rbc,tsx)
rbcw.ret <- rbcw.ret[-1,]
tsxw.ret <- tsxw.ret[-1,]
tbills <- tbills[-1,]
cad_us <- cad_us[-1,]

index(rbcw.ret) -> index(tsxw.ret) -> index(tbills) -> index(cad_us)
data <- cbind(rbcw.ret,tsxw.ret,tbills,cad_us)
colnames(data) <- c("Rbc Returns","TSX Returns", "Tbill","CAD/US")
rm(rbcw.ret,tsxw.ret,tbills,cad_us)

data <- as.data.frame(data)
capm1 <- lm_robust(data$`Rbc Returns`~ data$`TSX Returns` + data$`Tbill` + data$`CAD/US`)

#Computing the var
dev_market <- capm1$std.error[2]
dev_intrate <- capm1$std.error[4]
dev_bill <- capm1$std.error[3]

beta_market <- capm1$coefficients[2]
beta_intrate <- capm1$coefficients[4]
beta_bill <- capm1$coefficients[3]
c <- 2.326
VaR <- c(sqrt((beta_market*dev_market)^2 + (beta_intrate*dev_intrate)^2 + (beta_bill*dev_bill)^2))


