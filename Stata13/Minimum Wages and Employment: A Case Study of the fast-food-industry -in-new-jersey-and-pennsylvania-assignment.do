
#I will not post the data here. If interested, please do not hesitate to contact me :) 
drop shore ncalls inctime firstinc pctaff open nregs nregs11 inctime2 firstin2 open2r date2 type2
tab chain, gen(f)
gen bk =1 if f1==1
replace bk=0 if f1==0
label var bk "Burger King"
gen kfc=1 if f2==1
replace kfc=0 if f2==0
label var kfc "KFC"
gen rr=1 if f3==1
replace rr=1 if f3==0
label var rr "Roy Rogers"
gen w=1 if f4==1
label var w "Wendy's"
replace w=0 if f4==0
*** To generate FTE for two waves
gen FTEw1 = empft+nmgrs+0.5*emppt 
label var FTEw1 "Full Time Equivalent, wave 1"
gen FTEw2 = empft2+nmgrs2+0.5*emppt2 
label var FTEw2 "Full Time Equivalent, wave 2"
ttest FTEw1, by(state) unequal
ttest FTEw2, by(state) unequal 
*** To generate Percentage full time employees
gen pftew1 = [empft/(FTEw1)]*100 
label var pftew1 "Percentage full-time employees, wave 1"
gen pftew2 = [empft2/(FTEw2)]*100
label var pftew2 "Percentage full-time employees, wave 2"
ttest pftew1, by(state) unequal
ttest pftew2, by(state) unequal
***Starting wage
replace wage_st =0 if wage_st ==.
replace wage_st2 =0 if wage_st2 ==.
ttest wage_st, by(state) unequal
***Wage= $4.25
gen wagew1425=1 if wage_st==4.25
replace wagew1425=0 if wage_st!=4.25
ttest wagew1425,by(state)
gen wagew2425=1 if wage_st2==4.25
replace wagew2425=0 if wage_st2!=4.25
ttest wagew2425,by(state)
tab wage_st2,gen(m)
gen wagew2505=1 if m12==1
replace wagew2505=0 if m12==0
drop m1-m24
ttest wagew2505,by(state)
***Price of Full Meal***
gen fmpw1= psoda+pfry+pentree
label var fmpw1 "Price of full meal  wave 1"
gen fmpw2= psoda2+pfry2+pentree2
label var fmpw2 "Price of full meal, wave 2"
ttest fmpw1, by(state) unequal
ttest fmpw2, by(state) unequal
***Hours of Operation***
ttest hrsopen, by(state) unequal 
ttest hrsopen2, by(state) unequal
***Recruiting Bonus***
ttest bonus, by(state) unequal
ttest special2, by(state) unequal
//Table 3
***To get lines 1-3 "Stores by state"
egen FTEw1PA=mean(FTEw1) if state==0 // First line first column
egen FTEw1NJ=mean(FTEw1) if state==1 //Second line first column
egen FTEw2PA=mean(FTEw2) if state==0  // First line second column
egen FTEw2NJ=mean(FTEw2) if state==1 //Second line second column
replace FTEw1NJ=20.43941 if FTEw1NJ==.
replace FTEw2NJ=21.02743 if FTEw2NJ==.
replace FTEw1PA=23.33117 if FTEw1PA==.
replace FTEw2PA=21.16558 if FTEw2PA==.
label var FTEw1NJ "FTE before, NJ"
label var FTEw1PA "FTE before, PA"
label var FTEw2NJ "FTE after , NJ"
label var FTEw2PA "FTE after, PA"
gen dFTEb = FTEw1NJ-FTEw1PA  //First line, third column
label var dFTEb "The difference between  states before treatment"
gen dFTEa = FTEw2NJ-FTEw2PA //Second line, third column
label var dFTEa "The difference between states after treatment"
gen tfteNJ = FTEw2NJ-FTEw1NJ  //Third row, second column
label var tfteNJ "The difference between before/after within NJ"
gen tftePA = FTEw2PA-FTEw1PA //Third row, first column
label var tftePA "The difference between before/after within PA"
gen dfteNJPA = tfteNJ-tftePA // Third row, third column
label var dfteNJPA "The difference in difference effect"
*********************************************************
//Lines 1-3 "Stores in New Jersey"
//Wage = $4.25
egen FTEw1NJ11=mean(FTEw1) if state==1 & wage_st==4.25 //First row, fourth column
egen FTEw2NJ22=mean(FTEw2) if state==1 & wage_st==4.25  //Second row, fourth column
replace FTEw1NJ11 =19.5567 if FTEw1NJ11==.
replace FTEw2NJ22 =20.87755 if FTEw2NJ22==.
label var FTEw1NJ11 "FTE before, wage =$4.25"
label var FTEw2NJ22 "FTE after, wage =$4.25"
//Wage = $4.26-4.99
egen FTEw1NJ111=mean(FTEw1) if state==1 & wage_st>4.25 & wage_st<5.00 //First row, fifth column
egen FTEw2NJ222=mean(FTEw2) if state==1 & wage_st>4.25 & wage_st<5.00  //Second row, fifth column
replace FTEw1NJ111 =20.08066 if FTEw1NJ111==.
replace FTEw2NJ222 =20.95555 if FTEw2NJ222==.
//wage >5.00
egen FTEw1NJ1111=mean(FTEw1) if state==1 & wage_st>=5.00 //First row, sixth column
egen FTEw2NJ2222=mean(FTEw2) if state==1 & wage_st>=5.00 // Second row, sixth column
replace FTEw1NJ1111 =22.25 if FTEw1NJ1111==.
replace FTEw2NJ2222 =20.21 if FTEw2NJ2222==.
label var FTEw1NJ1111 "FTE before, wage>=$5.00"
label var FTEw2NJ2222 "FTE after, wage>=$5.00"
label var FTEw1NJ111 "FTE before, midrange wage"
label var FTEw2NJ222 "FTE after, midrange wage"
gen dmfte425 = FTEw2NJ22 - FTEw1NJ11 //Third row, foruth column
label var dmfte425 "The difference in FTE before and after treatment,NJ, sample with wages $4.25"
gen dmfte426499 = FTEw2NJ222 - FTEw1NJ111 // Third row, fifth column
label var dmfte426499 "The difference in FTE before and after treatment,NY, sample with wages $4.26-4.99"
gen dmfte500 = FTEw2NJ2222 - FTEw1NJ1111 //Third row, sixth column
label var dmfte500 "The difference in FTE before and after treatment, NY, sample with wages $5.00 and above"
//Lines 1-3 "Differences within NJ"
gen difflhb = FTEw1NJ11 - FTEw1NJ1111 //First row, seventh column
label var difflhb "The difference between $4.25 and $5.00 samples, FTE, before treatment. Difference within NJ" 
gen difflha = FTEw2NJ22 - FTEw2NJ2222 // Second row, sevdnth column
label var difflha "The difference between $4.25 and $5.00 samples, FTE, after treatment. Difference within NJ" 
gen diffmhb = FTEw1NJ111 - FTEw1NJ1111 //First row,eighth column
label var diffmhb "The difference between midrange and $5.00 samples, FTE, before treatment. Difference within NJ" 
gen diffmha = FTEw2NJ222 - FTEw2NJ2222 //Second row,eighth column
label var diffmha "The difference between midrange and $5.00 samples, FTE, after treatment. Difference within NJ" 
gen difindiflh = difflha - difflhb //Third row, seventh column
label var difindiflh "The difference in difference between $4.25 and $5.00 samples, before and after effect"
gen difindifmh = diffmha - diffmhb //Third row, eight column
label var difindifmh "The difference in difference between midrange and $5.00 samples, before and after effect"

****LINE 4********************************************************************** 
//NEW SAMPLE
// This part is done with some discrepancy, due to inability to drop appropriate variables.  Once they are dropped I cannot construct line 5 of the table 3. So, I have decided to sacrifice. 
drop if empft==.
drop if emppt==.
drop if nmgrs==.
egen FTEw1NJ1=mean(FTEw1) if state==1 
egen FTEw1PA1=mean(FTEw1) if state==0 
egen FTEw2NJ1=mean(FTEw2) if state==1 
egen FTEw2PA1=mean(FTEw2) if state==0 
replace FTEw1NJ1=20.43941 if FTEw1NJ1==.
replace FTEw1PA1=23.33117 if FTEw1PA1==.
replace FTEw2NJ1=20.89725 if FTEw2NJ1==.
replace FTEw2PA1=21.09667 if FTEw2PA1==.
label var FTEw1NJ1 "FTE before, subset, NJ"
label var FTEw2NJ1 "FTE after, subset, NJ"
label var FTEw1PA1 "FTE before, subset, PA"
label var FTEw2PA1 "FTE after, subset, NJ"
gen dFTEb1 = FTEw1NJ1-FTEw1PA1
label var dFTEb1 "The difference between states, pre-treatment, subset."
gen dFTEa1 = FTEw2NJ1-FTEw2PA1
label var dFTEa1 "The difference between states, post-treatment, subset."
gen tfteNJ1 = FTEw2NJ1-FTEw1NJ1 //Row 4, Column 2 (discr = 0.02)
label var tfteNJ1 "The difference between treated and non-treated NJ,subset."
gen tftePA1 = FTEw2PA1-FTEw1PA1 //Row 4, Column 1 (discr = 0.05)
label var tftePA1 "The difference between treated and non-treated PA,subset." 
gen dfteNJPA1 = tfteNJ1-tftePA1 //Row 4, Column 3 (discr = 0.06)
label var dfteNJPA1 "The difference-in difference between states, subset."
drop FTEw1NJ1 FTEw1PA1 FTEw2PA1 FTEw2NJ1 dFTEb1 dFTEa1 
//wage=$4.25
egen FTEw1NJ_11=mean(FTEw1) if state==1 & wage_st==4.25
egen FTEw2NJ_22=mean(FTEw2) if state==1 & wage_st==4.25
replace FTEw1NJ_11 =19.5567 if FTEw1NJ_11==.
replace FTEw2NJ_22 =20.64894 if FTEw2NJ_22==.
label var FTEw1NJ_11 "FTE before, new sample, wage =$4.25"
label var FTEw2NJ_22 "FTE after, new sample, wage =$4.25"
gen dFTE_1 = FTEw2NJ_22-FTEw1NJ_11 //Row 4, column 4 , Discrepancy of 0.12.
label var dFTE_1 "The difference in FTE, new sample, wage=$4.25"
//wage = $4.26-4.99
egen FTEw1NJ_111=mean(FTEw1) if state==1 & wage_st>4.25 & wage_st<5.00
egen FTEw2NJ_222=mean(FTEw2) if state==1 & wage_st>4.25 & wage_st<5.00
replace FTEw1NJ_111 =20.20303 if FTEw1NJ_111==.
replace FTEw2NJ_222 =20.91288 if FTEw2NJ_222==.
label var FTEw1NJ_111 "FTE before, new sample, midrange wage"
label var FTEw2NJ_222 "FTE after, new sample, midrange wage"
gen dFTE_2 = FTEw2NJ_222-FTEw1NJ_111 //Row 4, column 5
label var dFTE_2 "The difference in FTE, new sample, midrange wage"
//wage = $5.00
egen FTEw1NJ_1111=mean(FTEw1) if state==1 & wage_st>=5.00
label var FTEw1NJ_1111 "FTE before, new sample, wage above $5.00"
replace FTEw1NJ_1111=22.16791 if FTEw1NJ_1111==.
egen FTEw2NJ_2222=mean(FTEw2) if state==1 & wage_st>=5.00
label var FTEw2NJ_2222 "FTE before, new sample, wage above $5.00"
replace FTEw2NJ_2222=20.011191 if FTEw2NJ_2222==.
gen dFTE_3 = FTEw2NJ_2222-FTEw1NJ_1111
label var dFTE_3 "The difference in FTE, new sample, wage above $5.00" //Row 4, column 6
gen difflh_1 =dFTE_1-dFTE_3 //Row 4, column 7
label var difflh_1 "The difference between low wage and high wage, new sample"
gen difflh_2 = dFTE_2-dFTE_3 //Row 4, column 8
label var difflh_2 "The difference between low wage and high wage, new sample"
drop FTEw1NJ_11 FTEw2NJ_22 FTEw1NJ_111 FTEw2NJ_222 FTEw1NJ_1111 FTEw2NJ_2222
**Line 5**********************************************************************
tab status2,gen(m)
replace FTEw2=0 if m6==1
replace FTEw2=0 if m5==1
replace FTEw2=0 if m3==1
egen FTEw1NJ2=mean(FTEw1) if state==1 
egen FTEw1PA2=mean(FTEw1) if state==0
egen FTEw2NJ2=mean(FTEw2) if state==1 
egen FTEw2PA2=mean(FTEw2) if state==0 
replace FTEw1NJ2=20.43941 if FTEw1NJ2==.
replace FTEw2NJ2=20.630193 if FTEw2NJ2==.
replace FTEw1PA2=23.33117 if FTEw1PA2==.
replace FTEw2PA2=21.09667 if FTEw2PA2==. 
gen dFTE_4pa = FTEw2PA2- FTEw1PA2 //Row 5 , column 1 Dicrepency 0.05
label var dFTE_4pa "The difference between treated and non treated sample for PA, FTE, sample2"
gen dFTE_4nj = FTEw2NJ2 - FTEw1NJ2  //Row 5, column 2 dicrepency 0.04
label var  dFTE_4nj  "The difference between treated and non treated sample for NJ, FTE, sample2"
gen dFTE_4 = dFTE_4nj - dFTE_4pa  //Row 5, column 3 dicrepency 0.09
label var dFTE_4 "The difference in difference between PA and NJ, sample2, FTE"
drop FTEw1NJ2 FTEw2NJ2 FTEw1PA2 FTEw2PA2
//wage = $4.25
egen FTEw1NJ_112=mean(FTEw1) if state==1 & wage_st==4.25
egen FTEw2NJ_221=mean(FTEw2) if state==1 & wage_st==4.25
replace FTEw1NJ_112 =19.5567 if FTEw1NJ_112==.
replace FTEw2NJ_221= 20.43158 if FTEw2NJ_221==.
gen dFTE1_1 = FTEw2NJ_221-FTEw1NJ_112  //Row 5, column 4 Discrepency 0.03
drop FTEw1NJ_112 FTEw2NJ_221
label var dFTE1_1 "The difference in FTE, new sample2, wage $4.25"
//wage = $4.26-$4.99
egen FTEw1NJ_1112=mean(FTEw1) if state==1 & wage_st>4.25 & wage_st<5.00
egen FTEw2NJ_2221=mean(FTEw2) if state==1 & wage_st>4.25 & wage_st<5.00
replace FTEw1NJ_1112 = 20.08066 if FTEw1NJ_1112==.
replace FTEw2NJ_2221 = 20.60075 if FTEw2NJ_2221==.
gen dFTE1_2 = FTEw2NJ_2221- FTEw1NJ_1112 // Row 5, column 5. Dicrepency 0.03
drop FTEw1NJ_1112 FTEw2NJ_2221
label var dFTE1_2 "The difference in FTE, new sample2, wage $4.26-$4.99"
//wage = $5.00 and above
egen FTEw1NJ_111112=mean(FTEw1) if state==1 & wage_st>=5.00
label var FTEw1NJ_111112 "FTE before, new sample2, wage above $5.00"
replace FTEw1NJ_111112=22.25 if FTEw1NJ_111112==.
egen FTEw2NJ_22221=mean(FTEw2) if state==1 & wage_st>=5.00
label var FTEw2NJ_22221 "FTE before, new sample2, wage above $5.00"
replace FTEw2NJ_22221=19.71691 if FTEw2NJ_22221==.
gen dFTE1_3 = FTEw2NJ_22221-FTEw1NJ_111112 //Row 5, column 6 , dicrepency 0.14
drop FTEw1NJ_111112 FTEw2NJ_22221
label var dFTE1_3 "The difference in FTE, new sample2, wage above $5.00"
gen difflh_11 = dFTE1_1 - dFTE1_3 //Row 5, column 7, dicrp 0.04
label var difflh_11 "The difference in FTE between low wage and high wage sample2"
gen diffmh_11 = dFTE1_2 - dFTE1_3 //Row 5, column 8, dicrp 0.17
label var diffmh_11 "The difference in FTE between midrange  and high wage sample2"
****TABLE 4*******************************************************************************************
//To arrive to approx sample of the paper. This gives 351 observed results. Paper uses 357.
drop if empft2==.
drop if emppt2==.
drop if nmgrs2==.
drop if empft==.
drop if emppt==.
drop if nmgrs==.
drop if wage_st==.
drop if wage_st2==.
drop if wage_st==0
drop if wage_st2==0
gen changeE= FTEw2-FTEw1
reg changeE state
eststo
tab chain, gen (k)
reg changeE state k1-k4 co_owned
eststo
gen GAP=.
replace GAP = (5.05-wage_st)/wage_st
replace GAP=0 if state==0
replace GAP= 0 if wage_st >=5.05
reg changeE GAP
eststo
reg changeE GAP k1-k4 co_owned
eststo 
reg changeE  k1-k4 co_owned GAP centralj northj pa1 pa2
eststo 
esttab, se r2 nocons

