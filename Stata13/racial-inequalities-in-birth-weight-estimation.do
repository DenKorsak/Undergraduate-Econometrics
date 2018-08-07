
use ""
cap program drop Ass2
program define Ass2
quietly set more off
quietly replace cig_0=. if cig_0==99
di "Question 1"
tabstat month mager cig_0 dbwt boy smoke smoker prenatal, statistics( mean sd ) by(race)
di"Health Variables by race. Race 1-Whites, 2-Blacks, 3-Mexicans, 4-Aisans"
tabstat married educ primary secondary tertiary, statistics( mean sd ) by(race)
di"Socio-economic variables by race. Race 1-Whites, 2-Blacks, 3-Mexicans, 4-Aisans"
di _n
di "Question 2"
di _n
di "Part (a)"
di _n
quietly eststo clear
quietly reg dbwt black mexican asian
quietly eststo
quietly reg dbwt black mexican white asian , nocons
quietly eststo
esttab, se r2  label title(Summary Table for Mr.Hildebrand(1))
di _n
di "Part (b)"
di _n
di" Working with Summary Table for Mr.Hildebrand(1).In the regression (1) our constant represents an average weight of a child born from Non-Hispanic White mother. Hence, Non-Hispanic Whites are the reference group. The coefficients may be interpreted as a gap between average weight of a new born of a certain race compared to the Whites. The regression (2) is done without a constant in order to avoid multicollinearity - dummy trap. In essence two models are the same.  In the regression (2) we can observe the values for the mean weight of a new born for each racial group.Our coefficients are 100% precise.To show it:"
quietly eststo clear
tabstat dbwt if black==1
tabstat dbwt if mexican==1
tabstat dbwt if asian==1
tabstat dbwt if white==1
di _n
di "Part (c(i))"
di _n
di "To capture seasonality, I would like to use dummy variables for each month in my regression."
quietly eststo clear
quietly tab month, gen(m)
quietly reg dbwt black mexican asian m2-m12
quietly label var m1 January 
quietly label var m2 February
quietly label var m3 March
quietly label var m4 April
quietly label var m5 May
quietly label var m6 June
quietly label var m7 July
quietly label var m8 August
quietly label var m9 September
quietly label var m10 October
quietly label var m11 November
quietly label var m12 December
quietly eststo
quietly reg dbwt black mexican asian m1-m12, nocons
quietly eststo
esttab, se r2 label title(Summary Table for Mr.Hildebrand(2))
quietly eststo clear
di _n
di "Working with Summary Table for Mr.Hildebrand(2). The intercept presents the average weight of a child born from a white mother in month 1=(January - our reference group) plus the premium (negative or positive) from the fact that this child is born not white in the month of January plus the premium that he is born white. This happens since I control for all the races except whites and for all the months except January and the relationship between seasons and races is cyclical (x affects y, y affects x in return).  Further, I do the same regression without a constant to show this concept. Lastly, in the table one, we can see the additional premium of a child to be born in a certain month.  Month of June has the highest impact on average birth weight controlling for racial groups. By the nature of the construction of the OLS on dummy variables, when the child born in June (i.e. m6==1)it alters our intercept and has an impact of 17.84141 grams, therefore, the mean weight of a child born in June increases by 17.8414 on average. The lowest coefficient of a dummy variable obtained is (m12==month) of December. It may be similarly interpreted; however, the sign of the effect differs."
di _n
di _n
di "Part (c(ii))"
di _n
quietly eststo clear
quietly gen decju=0
quietly replace decju=1 if(m6+m12)
quietly regress dbwt black mexican asian m2-m5 m7-m12 decju 
esttab, se r2 label title(Summary Table for Mr.Hildebrand(3))
di _n
quietly eststo clear
di _n
di"Working with Summary Table for Mr.Hildebrand(3). We reject the null hypothesis that the coefficient decju is zero at 5% significance level. Hence, the difference is statistically significant at 5% level."
di _n
di "Part (c(iii))"
di _n
di "In general, R squared measures the distance between the data and the fitted regression line. It is measured as explained variation over total variation ( 0<x<1, where x is R squared). For this regression, it is really close to zero. This would mean that data points lie far from the fitted regression line. Therefore, this model does not capture a lot of variation in the data sample."
di _n 
di"Question 3"
di _n
di "Part (a)"
di _n
di"I had dropped Mexicans and Asians since the task now is to focus on disparities between Whites and Blacks."
quietly drop asian mexican
quietly drop if white==0 & black==0
quietly eststo clear
quietly reg dbwt black
quietly eststo
quietly reg dbwt black mager primary secondary tertiary
quietly eststo
esttab, se r2 label title(Summary Table for Mr.Hildebrand(4))
di _n
di "Working with Summary Table for Mr.Hildebrand (4). We now see the value of the coefficient for blacks is lower by (250-227.6). Therefore, the gap had decreased. Our new coefficient represents the relationship of the independent variable black to the dbwt netted out of correlated parts between blacks/education and blacks/age.  As we control for more effects that have impact on average birth weight (such as education or age), our coefficient for blacks would become more precise (We had taken these variables from U(population parameter of the error term in our model) and control for them in our regression. Our R squared remains to be low. Our constant term in the model(2) does not represent average weight a new born from a Non-Hispanic White mother anymore(as it does in the model(1)). We reject the null hypothesis that coefficient equals to zero for each coefficient at 10% significance level(most of them are significant at 1% and 5%). "
di _n
di "Part (b)"
quietly eststo clear
quietly gen bprimary=0
quietly gen bsecondary=0
quietly gen btertiary=0
quietly gen wprimary=0
quietly gen wsecondary=0
quietly gen wtertiary=0
quietly replace wprimary=white*primary
quietly replace wsecondary=white*secondary
quietly replace wtertiary=white*tertiary
quietly replace bprimary=black*primary
quietly replace bsecondary=black*secondary
quietly replace btertiary=black*tertiary
quietly label var wprimary WhitesPrimaryEduc
quietly label var wsecondary WhitesSecondaryEduc
quietly label var wtertiary WhitesTertiaryEduc
quietly label var bprimary BlacksPrimaryEduc
quietly label var bsecondary BlacksSecondaryEduc
quietly label var btertiary BlacksTertiaryEduc
quietly reg dbwt bprimary bsecondary btertiary wprimary wsecondary wtertiary, nocons
quietly eststo
di _n
esttab, se r2 label title(Summary Table for Mr.Hildebrand(5))
di _n
di" Working with Summary Table for Mr.Hidlebrand(5). We have 132393 observations participating in this regression. Each coefficient is statistically significant at 1% significance level. Since we regress with no constant, there is no reference group. This regression output presents the concrete numbers for average baby weight controlling for educational achievement of a mother for each of two races. In another words, this determines the average weight of a baby born from a white or a black with a certain level of education."
di _n
quietly eststo clear
di _n
di "Part (c)"
quietly reg dbwt black mager primary secondary tertiary smoker prenatal
quietly eststo
quietly reg dbwt black mager primary secondary tertiary
quietly eststo
esttab, se r2 label title(Summary Table for Mr.Hildebrand(6))
di _n
di "The point estimate of the racial birth weight gap is not very different. I observe difference of 3.65 grams."
di _n
quietly eststo clear
di _n
di"Part (d)"
quietly reg black mager primary secondary tertiary smoker prenatal 
quietly predict black_predict if e(sample) 
quietly predict black_resid, res 
quietly reg dbwt black_resid
quietly eststo
quietly reg dbwt black mager educ smoker prenatal
quietly eststo
esttab, se r2 label title(Summary Table for Mr.Hildebrand(7))
di _n
quietly eststo clear
end
log using "C:\Users\denis95\Downloads\econ4260_us_bwght_2015.log",name(Assignment_2_Econometrics) replace 
Ass2
view ".log"



