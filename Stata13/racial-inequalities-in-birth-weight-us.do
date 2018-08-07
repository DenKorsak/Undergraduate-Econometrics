use ""
log using ""
set more off
tabulate mbrace, gen (a)
tabulate mracehisp, gen (b)
tabulate mhisp_r, gen (c)
generate nhw=0
replace nhw=1 if a1==1 & b1==1 & c1==1
label var nhw "Non-Hispanic Whites"
generate blck=0
replace blck=1 if (a2==1 & (b2==1 | c1==1))
label var blck "Blacks"
generate mxc=0
replace mxc=1 if (a1==1 & b7==1 & c2==1)
label var mxc "Mexicans"
tabulate mrace15, gen (d)
generate asian=0
replace asian=1 if(d5==1 | d6==1 | d7==1 | d8==1 | d9==1)
label var asian "Asians"
gen race= "Whites"
replace race = "Asians" if (asian==1)
replace race = "Mexicans" if (mxc==1)
replace race = "Blacks" if (blck==1)
drop a1-c7
drop d1-d16
label var race race
tabulate dmar,gen(e)
label var e1 "married"
drop e2
rename e1 MStatus
tabulate sex, gen (a)
drop a1
label var a2 "Sex of the infant"
rename a2 boy
tabulate meduc,gen(a)
 gen educ=1 if (a1==1 | a2==1)
. replace educ=2 if (a3==1 | a4==1)
. replace educ=3 if (a4==1 | a5==1 | a6==1 | a7==1 | a8==1 | a9==1)
. replace educ=3 if (a4==1 | a5==1 | a6==1 | a7==1 | a8==1)
. replace educ=0 if (a9==1)
gen primary = 0
replace primary=1 if educ==1
gen secondary=0.
replace secondary=1 if educ==2
gen tertiary=0
replace tertiary=1 if educ==3
drop a1-a9
label var educ "Level of Education"
label var primary "Not completed High School"
label var secondary "Completed High School or some college credits"
label var tertiary "Ph.D, M.A., B.A, A.S, A.A"

rename nhw white
rename blck black
rename mxc mexican
rename asian asian
rename MStatus married

egen smoke=rmean( cig_1 cig_2 cig_3 )
gen smoker = 0
replace smoker = 1 if (smoke > 0)
replace smoker = . if (smoke == .)
gen prenatal = previs
replace prenatal =. if (prenatal==99)
rename dob_mm month

drop dob_yy bfacil bfacil3 mager restatus mrace15 mbrace mraceimp mhisp_r mracehisp mar_p dmar mar_imp meduc previs cig_1 cig_2 cig_3 cig_rec rdmeth_rec apgar5 sex gestrec10 ubfacil




di "Question 2 (a)"
mean dbwt 
mean dbwt if (primary==1)
mean dbwt if (secondary==1)
mean dbwt if (tertiary==1)


di "We can observe that as the level of education rises there is a very significant increase of a child weight (200 grams). Further, if the level of education is primary then the weight of a child is significantly below the mean. As the level of education rises, the weight approaches and sucessfully surpasses the mean value of the child-weight. Note: I would like to point out that deal with a 5% sample of a standard sample of a population. Therefore, one might ask for at least a bigger sample to establish a strong inference."

di "Question 2 (b)"
mean dbwt if (white==1)
mean dbwt if (black==1)
mean dbwt if (mexican==1)
mean dbwt if (asian==1)


di "Question 2 (c)"
di "White"
mean dbwt if (primary==1 & white==1)
mean dbwt if (secondary==1 & white==1)
mean dbwt if (tertiary==1 & white==1)
di "Black"
mean dbwt if (primary==1 & black==1)
mean dbwt if (secondary==1 & black==1)
mean dbwt if (tertiary==1 & black==1)
di "Mexicans"
mean dbwt if (primary==1 & mexican==1)
mean dbwt if (secondary==1 & mexican==1)
mean dbwt if (tertiary==1 & mexican==1)
di "Asians"
mean dbwt if (primary==1 & asian==1)
mean dbwt if (secondary==1 & asian==1)
mean dbwt if (tertiary==1 & asian==1)

di "Brief Comments"
di "We clearly see that for groups white and black the mean value weight of a new-born rises with the level of education.  Here, I think we could argue a positive relationship of education to child weight in the sample.  We clearly see the increase of the mean value of the variable of interest when we compare primary and tertiary education levels. The relationship is ambiguous for the Asians and Mexicans. The mean value weight of a new born falls with the increase of education for Asians, but the decrease is really insignificant. Plus, the sample size for Asians with primary and secondary education is small (334 and 857) relative to other racial groups.  The weight of a child born Asian is close to the average mean value. It is hard to establish any relationship here. The mean value weight falls and then rises with the increase of education for Mexicans.  Hard to get any inference here either, the weight rises if compare primary to tertiary education level, but not on significant amount (10 g). "
di "Question 2 (c)"

mean dbwt if (smoker == 0)                     
mean dbwt if (smoker == 0 & cig_0>0)              
mean dbwt if (smoker == 1 & cig_0>0)
di "Comments"
di "We observe that the mean weight of a new-born is higher when the mother is a non-smoker (3292.969>3122.178). I would not comfortably argue that there is a negative relationship between the continuous (i.e. people who did not quit during the pregnancy) smoking and the average bodyweight of a new-born. The reason is that we do not deal with two comparable sample sizes (179276 non-smokers vs 16477 cont.smokers only).  Mean value weight of a child of a non-smoking or quit-smoking mother is indeed higher that of a continuous smoking mother. However, we would need more data on smoking individuals to establish a clear inference. The average weight of a child of a mother who quit smoking before pregnancy is 17 grams greater on average than of a non-smoking mother. Hard to explain it. We should also notice that the sample size of continuous smokers is much smaller than of non-smoking mothers. (4845  vs  179276). Given the well know influence of smoking on the health of individual, I would argue that the peculiarity of this sample size is the main reason for such data to appear.  To summarize, by looking at numbers I may identify a negative association between continuously smoking mother and non-smoking mother. However, I would be careful to establish a causal inference relationship due to the incomparability of sample sizes."
log off

