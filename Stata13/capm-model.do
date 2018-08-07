##I will not post the data here. If interested, do not hesitate to contact me! :) 

qui import excel "C:\datasp.xlsx",first clear	
qui gen day=mofd(Date)
qui gen year = year(Date)
qui gen month = month(Date)
qui format day %tm
qui drop Date High Low Close Volume
qui rename AdjClose adjclosesp
qui label var adjclosesp adjclosesp
qui save \Users\denis95\Downloads\datasp.dta,replace
qui clear

qui import excel "\datarbc.xlsx", sheet("RY.TO") firstrow
qui gen day=mofd(Date)
qui gen year = year(Date)
qui gen month = month(Date)
qui format day %tm
qui drop  Date High Low Close Volume Open
qui rename AdjClose adjcloserbc
qui label var adjcloserbc adjcloserbc
qui save \Users\denis95\Downloads\datarbc.dta,replace
qui clear

qui import excel "C:\tbills.xls", firstrow
qui gen day=mofd(Date)
qui gen year = year(Date)
qui gen month = month(Date)
qui format day %tm
qui drop Date
qui rename Value yield
label var yield "yield on 3 month T-Bill Risk free"
qui save \Users\denis95\Downloads\datayield.dta,replace
qui clear

qui use \Users\denis95\Downloads\datasp.dta
qui merge 1:1 day using \Users\denis95\Downloads\datayield.dta
qui drop _merge 
qui save \Users\denis95\Downloads\datayieldandsp.dta
qui clear
qui use \Users\denis95\Downloads\datayieldandsp.dta
qui merge 1:1 day using \Users\denis95\Downloads\datarbc.dta
qui drop _merge Open day 
qui save \Users\denis95\Downloads\readydata.dta,replace

cap program drop ass3
qui program define ass3
qui set more off
qui import excel using "", firstrow 
//2.A)
di "Alpha coefficient represents a parameter in the capital asset pricing model. It is the intercept of the security characteristic line (regression line), in another words, the coefficient of the constant in a market model regression. The alpha coefficient indicates how an investment has performed after accounting for the risk it involved(a<0 => earned too little for its risk, a>0 => excess return for its risk, a=0 => adequate return for its risk)."
di "Beta coefficient represents the sensitivity of the expected excess asset returns to the expected excess market returns. In another words, Beta is a measure of the volatility, or systematic risk, of a security or a portfolio in comparison to the market as a whole. A beta of 1 indicates that the security's price moves with the market. A beta of less than 1 means that the security is theoretically less volatile than the market. A beta of greater than 1 indicates that the security's price is theoretically more volatile than the market. For example, if a stock's beta is 1.2, it's theoretically 20% more volatile than the market"
//2.B)
qui gen returnrbc = (adjcloserbc[_n]-adjcloserbc[_n-1])/adjcloserbc[_n-1]
qui replace returnrbc =0 if returnrbc ==.
qui label var returnrbc returnrbc
qui gen retrbcan = (returnrbc+1)^12-1  
qui label var retrbcan retrbcan
qui gen returnsp = (adjclosesp[_n]-adjclosesp[_n-1])/adjclosesp[_n-1]
qui replace returnsp =0 if returnsp ==.
qui label var returnsp returnsp
qui gen retspan = (returnsp+1)^12-1  
qui label var retspan retspan
qui gen yieldadj = yield/100
qui drop yield
qui gen returnrbc1 = retrbcan-yieldadj
qui label var returnrbc1  "Rbc risk premium"
qui gen returnsp1 = retspan-yieldadj
qui label var returnsp1  "Market risk premium"
qui replace yieldadj=0 if month==1 & year==2006
qui replace returnrbc1=0 if month==1 & year==2006
qui replace returnsp1=0 if month==1 & year==2006
//3.A)
eststo clear
qui reg returnrbc1 returnsp1 if inrange(year, 2006, 2008)
qui eststo
qui reg returnrbc1 returnsp1 if inrange(year, 2014, 2016)
qui eststo
esttab, se r2 label title(Samples 2006-2008 and 2014-2016)
//3.B)
qui eststo clear
qui reg returnrbc1 returnsp1 if inrange(year, 2006, 2008)
qui estimates store reg1
qui reg returnrbc1 returnsp1 if inrange(year, 2014, 2016)
qui estimates store reg2
qui suest reg1 reg2
test [reg1_mean]returnsp1 = [reg2_mean]returnsp1 = 0  
di "Prob>chi2 = 0.0000. This implies that we should reject the null hypothesis that two coefficients have the same power of the effect i.e. across two samples the beta coefficient from the regression brings a different magnitude of effect. "
//3.C)
di"For the time of the analysis, the beta coefficient listed on the Yahoo Finance web-site was 1.21"
qui reg returnrbc1 returnsp1 if inrange(year, 2014, 2016)
test returnsp1 = 1.21
di "We get prob>F = 0.4018. This implies that we fail to reject the null hypothesis that our beta coefficient is the same as provided by Yahoo Finance. Therefore, it is equal to 1.21"
//3.D)
test returnsp1 = 1
di "We get prob>F = 0.0917. This implies that we fail to reject the null hypothesis that our beta coefficient is equal to one i.e. fairly priced. However, two things are evident, first, the resulting Prob>F is very close to the rejection region at standart 5 % test, second, our sample size is only 36 obesrvations. The second fact may affect our test estimates "
//3.E)
test _cons=0
di "We get prob>F = 0.0564. This implies that we fail to reject the null hypothesis that our alpha coefficient is equal to zero. In another words, this asset earned a return adequate for the risk taken over the most recent observational period. Again, two things are evident, first, the resulting Prob>F is very close to the rejection region at standart 5 % test, second, our sample size is 36 obesrvations. The second fact most likely affects our test estimates."
//3.F)
quietly tab month, gen(m)
qui eststo clear
qui reg returnrbc1 returnsp1 m1-m12, nocons
qui eststo
esttab, se r2 label title(Sample 2014-2016)
di "If we disregard statistical insignificance of the effect, the additional premium to the return of the stock j is -0.16 if it is traded in January. Additional unit increase in S&P volume in January leads to decrease of RBC stock by 0.16 units. The January effect is a seasonal increase in stock prices during the month of January. Analysts generally attribute this rally to an increase in buying, which follows the drop in price that typically happens in December when investors, engaging in tax-loss harvesting to offset realized capital gains, prompt a sell-off. We may observe positive coefficient for December. This implies that the stocks were still getting additional premium for being traded in December (2014-2016), hence, no potential selloff yet. To speculate, in our sample, it is very likely that the January effect is transferred to February and to March. Maybe the selloff indeed happened but NOT in the month of December but in January (in December shareholders were still hesitant MAYBE). Hence, this explains why we obtain coefficients of a much greater positive magnitude for February (0.64) and March (0.81) and the decline in magnate afterwards. These coefficients are statistically significant at 5% and 1 % level. log using "
end
qui log using "log",name(capm)replace 
ass3
view ".log"

