library(quantmod)
library(Quandl)
library(xts)
library(dplyr)
library(hydroTSM)
library(PerformanceAnalytics)
library(stargazer)
library(car)
options(scipen=999)

#1.a,b,c)
getSymbols("^GSPTSE", src = "yahoo", auto.assign = FALSE) %>% window(, start = "2007-01-02", end = "2017-12-01") %>% daily2monthly(,FUN = sum) %>% as.xts -> sp500tsx
Quandl("BOC/V122531", type = "xts") %>% window(,start = "2007-01-02", end = "2017-12-01")  %>% as.xts -> tbills
getSymbols("RY.TO", src = "yahoo", auto.assign = FALSE) %>% window(,start = "2007-01-02", end = "2017-12-01") %>% daily2monthly(,FUN = sum) %>% as.xts -> rbc

#2. a)
print("Alpha coefficient represents a parameter in the capital asset pricing model.It is the intercept of the regression line. The alpha coefficient indicates how investment performs after accounting for risk. Alpha less than zero implies that it earned too little for risk taken. Alpha bigger than zero implies excess return for its risk, whereas  alpha equals to zero implies adequate return for its risk.")
print("Beta coefficient represents the sensitivity of the expected excess asset returns to the expected excess market returns.Beta is a measure of volatility of an asset relative to the market. A beta of 1 indicates that the security's price moves with the market. A beta of less than 1 means that the security is theoretically less volatile than the market. A beta of greater than 1 indicates that the security's price is theoretically more volatile than the market.")
      

#2. b)
ret <- function(x){(((1 + x)^12) - 1)}
rbc$RY.TO.Adjusted %>% Delt(,k=1) %>% ret -> return.rbc_an
sp500tsx$GSPTSE.Adjusted %>% Delt(,k=1) %>% ret -> return.sp500tsx_an
tbills/100 -> tbills.decf
merge(return.rbc_an,return.sp500tsx_an,tbills.decf) -> data
rm(rbc,return.rbc_an,return.sp500tsx_an,sp500tsx,tbills.decf,tbills)
data <- data[-1,]

#3. a)
data$rbc_risk.p <- data$Delt.1.arithmetic - data$tbills.decf
data$market_risk.p <- data$Delt.1.arithmetic.1 - data$tbills.decf
colnames(data) <- c("RBC","SP500TSX","Tbills","Rbc Risk Premium","Market Risk Premium")
subset.1<- data["2007-02-01/2010-02-01"]
subset.2 <- data["2014-02-01/2017-02-01"]
lm.1 <- lm(subset.1$`Rbc Risk Premium` ~ subset.1$`Market Risk Premium`)
lm.2 <- lm(subset.2$`Rbc Risk Premium` ~ subset.2$`Market Risk Premium` )
stargazer(lm.1,lm.2,type ="text", title = "Regression Results", omit.stat = c("LL","AIC", "BIC"), dep.var.labels = c("RBC Risk Premium", "RBC Risk Premium"), covariate.labels = c("Market Risk Premium","Market Risk Premium"),column.labels = c("1st interval","2nd interval"))

#3. b)
cov.b1b2 <- cov(subset.1$`Market Risk Premium`,subset.2$`Market Risk Premium`)
z.value <- (lm.1[["coefficients"]][["subset.1$`Market Risk Premium`"]] - lm.2[["coefficients"]][["subset.2$`Market Risk Premium`"]])/((0.3924 + 0.05506) - sqrt(cov.b1b2))  
if (abs(z.value) < 1.96) {print("we fail to reject the null hypothesis that Betas are equal across time periods")} else {"we reject the null hypothesis that Betas are equal across time periods"}
#3. c) 
test.2 <- linearHypothesis(lm.2, "subset.2$`Market Risk Premium` = 1.21") %>% print()
if (test.2$`Pr(>F)`[2] > 0.05) {print("we fail to reject the null hypothesis that Beta is equal to 1.21")} else {"we reject the null hypothesis that Beta is equal to 1.21"}
#3. d)
test.3 <- linearHypothesis(lm.2, "subset.2$`Market Risk Premium` = 1.00") %>% print()
if (test.3$`Pr(>F)`[2] > 0.05) {print("we fail to reject the null hypothesis that Beta is equal to 1.00")} else {"we reject the null hypothesis that Beta is equal to 1.00"}
#3. e)
test.4 <- linearHypothesis(lm.2, "(Intercept) = 0") %>% print()
if (test.4$`Pr(>F)`[2] > 0.05) {print("we fail to reject the null hypothesis that the intercept is equal to zero")} else {"we reject the null hypothesis that the intercept is equal to zero"}
#3. f)
data$dummy_jan = ifelse(months.Date(index(data)) == "January",1,0)
lm.3 <- lm(data$Rbc.Risk.Premium ~ data$dummy_jan)
stargazer(lm.1,lm.2,lm.3,type ="text", title = "Regression Results", omit.stat = c("LL","AIC", "BIC"), dep.var.labels = c("RBC Risk Premium", "RBC Risk Premium"), covariate.labels = c("Market Risk Premium","Market Risk Premium"),column.labels = c("1st interval","2nd interval"))
print("January effect is not significant at any significance level to variations of Rbc.Risk.Premium in our sample")

