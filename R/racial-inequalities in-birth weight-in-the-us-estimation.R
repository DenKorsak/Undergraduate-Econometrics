This is a 5% sample of Birth data for 2015 (currently the most recent birth data publicly available to download).

#Download packages
library(foreign)
library(graphics)
library(stats)
library(readstata13)
library(ggplot2)
library(dplyr)
library(pastecs)
library(tidyr)
library(stargazer)
#Import the data
#Notice the importance of generate.factors = TRUE. It assigns labels as White, Black, etc. to the variable race15. Without it, these labels will be displayed as numbers. 

mydata <- read.dta13("https://www.dropbox.com/s/0v65kwovv5xuv9t/econ4260_us_bwght_2015.dta?dl=1", convert.factors = TRUE, generate.factors = TRUE, encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,missing.type = FALSE, convert.dates = TRUE, replace.strl = TRUE,
add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL)


#Data preparation
#a)
mydata$race <- ifelse(mydata$mrace15 == "White (only)" & mydata$mhisp_r == "Non-Hispanic","Non-Hispanic Whites (NHW)",0) 
mydata$race <- ifelse(mydata$mrace15 == "Black (only)" & mydata$mhisp_r == "Non-Hispanic","Blacks",mydata$race)
mydata$race <- ifelse(mydata$mhisp_r == "Mexican" & mydata$mrace15 != "Black (only)","Mexicans", mydata$race )
mydata$race <- ifelse(mydata$mrace15 == "Chinese (only)" |mydata$mrace15 == "Japanese (only)" | mydata$mrace15 == "Filipino (only)" | mydata$mrace15 == "Vietnamese (only)"|mydata$mrace15 == "Korean (only)", "Asian", mydata$race)     

mydata$race <- as.factor(mydata$race)
mydata <- subset(mydata, mydata$race != 0)
mydata$race <- droplevels(mydata$race) #To remove unnecessary levels from the frame

options(scipen=999) #To remove scientific notation
plot(mydata$race, main = "Distribution of Races Across the Sample", xlab = "Races", ylab = "Amount of People", col = "Red")


#b)
#categorical variables for each race
mydata$white <- ifelse(mydata$race == "Non-Hispanic Whites (NHW)",1,0)
mydata$black <- ifelse(mydata$race == "Blacks",1,0)
mydata$asian <- ifelse(mydata$race == "Asian",1,0)
mydata$mexican <- ifelse(mydata$race == "Mexicans",1,0)

#categorical variable for martial status
mydata$married <- ifelse(mydata$dmar == "Yes",1,0)
#categorical variable for boy
mydata$boy <- ifelse(mydata$sex == "M",1,0)

#c)
#categorical variables for each education level. 
num_educ <- as.numeric(mydata$meduc)

mydata$educ <- ifelse(mydata$meduc == "8th grade or less"|mydata$meduc == "9th through 12th grade with no diploma",1,0)
mydata$educ <- ifelse(mydata$meduc == "High school graduate or GED completed"|mydata$meduc == "Some college credit, but not a degree",2,mydata$educ)
mydata$educ <- ifelse(num_educ == "8" |num_educ == "7"|num_educ == "6" | num_educ == "5",3,mydata$educ) 


#separated categorical variables for each level of education
mydata$primary <- ifelse(mydata$educ == 1,1,0)
mydata$secondary <- ifelse(mydata$educ == 2,1,0)
mydata$tertiary <- ifelse(mydata$educ == 3,1,0)

#Sort of equivalent to label define command in STATA13
mydata$educ <- factor(mydata$educ,levels = c(1,2,3),labels = c("Primary","Secondary","Tertiary")) 
plot(mydata$educ, main = "Distribution of Educational Achievement Across the Sample", xlab = "Level of Education", ylab = "Amount of People", col = "Green")

#d)
#Average level of smoking + smoker variable
mydata$smoke <- ((mydata$cig_1+mydata$cig_2+mydata$cig_3)/3)
mydata$smoker <- ifelse(mydata$smoke > 0, 1,0)


#e) Add missing values for 99
mydata$prenatal <- mydata$previs 
mydata$prenatal <- ifelse(mydata$prenatal == "99", NA, mydata$prenatal)

#f) 
#Rename variables
names(mydata)[names(mydata) == "dob_mm"] <- "month"
names(mydata)[names(mydata) =="race.mydata$race"] <- "race"

#g) 
#subset the data
mydata <- mydata[c("month","race","white","black","mexican","asian","married", "prenatal", "cig_0", "dbwt", "boy", "educ", "smoke", "smoker", "primary", "secondary", "tertiary","mager")]



#h)
#save dataset in R format
save(mydata, file = "us_dbwt_2015.Rda")



#Descriptive Statistics

#a) 

child_weight_by_education <- with(mydata, tapply(mydata$dbwt, list(Education = mydata$educ),mean)) %>% print()
print("We do  find statistically significant differences between three groups. In particular, the difference between primary and tertiary group is 139 gramms.")

#b)

child_weight_by_race <- with(mydata, tapply(mydata$dbwt, list(Race = mydata$race),mean)) %>% print()
print("Again, there is a significant difference in weight between race groups. In marticular, the difference between Black and White kids is the largest")

#c)

child_weight_by_race_and_education <- with(mydata, tapply(mydata$dbwt, list(Race = mydata$race,  Education = mydata$educ),mean)) %>% print()
print("We find statistically significant differences in childweight of each group excep Asians. The weight of a baby increases as the levele of education rises")


#d)
ifelse(mydata$cig_0 == 99, NA, mydata$cig_0)
mydata$smoking_mothers <- ifelse(mydata$cig_0 == 0 & mydata$smoker == 0, 0, 0)
mydata$smoking_mothers <- ifelse(mydata$cig_0 > 0 & mydata$smoker == 0, 1, mydata$smoking_mothers)
mydata$smoking_mothers <- ifelse(mydata$cig_0 != 0 & mydata$smoker > 0, 2, mydata$smoking_mothers) 

mydata$smoking_mothers <- factor(mydata$smoking_mothers,levels = c(0,1,2),labels = c("Never","Before_only", "Before_and_after")) 

child_weight_by_parental_smoking_factor <- with(mydata, tapply(mydata$dbwt, list(Smokers = mydata$smoking_mothers),mean)) %>% print()
plot(mydata$smoking_mothers, main = "Smokers and Non-Smokers", xlab = "Groups", ylab = "Amount of People", col = "Yellow")



round(child_weight_by_parental_smoking_factor, digits = 2) 
paste0("Birth weight of infants of non-smoking mothers :", round(mean(subset(mydata$dbwt, mydata$smoking_mothers == "Never")),digits = 2))
paste0("Birth weight of infants of mothers smoking before pregnancy only :", round(mean(subset(mydata$dbwt, mydata$smoking_mothers == "Before_only")),digits = 2))
paste0("Difference in Birth weight between non-smoking and smoking before:", round((mean(subset(mydata$dbwt, mydata$smoking_mothers == "Before_only"))) - (mean(subset(mydata$dbwt, mydata$smoking_mothers == "Never"))),digits = 2))
 
gr_1 <- subset(mydata$dbwt, mydata$smoking_mothers  == "Never")
gr_2 <- subset(mydata$dbwt, mydata$smoking_mothers == "Before_only")
gr_3 <- subset(mydata$dbwt, mydata$smoking_mothers == "Before_and_after") 



# For a Student test 
test_1 <- t.test(gr_1, gr_2, alternative = "two.sided", conf.level = 0.95, var.equal = FALSE)
if (test_1$p.value > 0.05) 
  {print("We cannot reject the null hypothesis that smoking is harmless at 5% significance level between non-smoker and smoker groups.")} else {"We reject the null hypothesis that smoking is harmless to infants birth weight at 5% level of significance between non-smoker and smoker groups."}

test_2 <- t.test(gr_1, gr_3, alternative = "two.sided", conf.level = 0.95, var.equal = FALSE)
if (test_2$p.value > 0.05) 
{print("We cannot reject the null hypothesis that smoking is harmless at 5% significance level between before only smokers and before and after smoker groups.")} else {"We reject the null hypothesis that smoking is harmless to infants birth weight at 5% level of significance between before only smokers and before and after smoker groups."}

rm(mydata, test_1,test_2,child_weight_by_education,child_weight_by_parental_smoking_factor,child_weight_by_race,child_weight_by_race_and_education, gr_1,gr_2,gr_3,num_educ) 

load(("us_dbwt_2015.Rda"))
#Descriptive table
mydata$num_educ <- as.numeric(mydata$educ)

health_vars_by_race <- mydata %>% select("mager","prenatal", "cig_0", "dbwt", "boy", "smoke", "smoker") %>% group_by(mydata$race) %>% na.omit %>% summarise_all(funs(mean)) %>% as.data.frame() 
col_1 <- health_vars_by_race[1]
health_vars_by_race <-round(health_vars_by_race[2:8], digits = 3) 
health_vars_by_race <- cbind(col_1,health_vars_by_race)
colnames(health_vars_by_race) <- c("Race","Av.Mage","Av.Prenatal Visits","Av.Cig.Before","Av.Child Weight","Av.Gender","Av.Smokers","Av.Still Smokers") 
print(health_vars_by_race)
#The way I found to round the digits. round() does not work 

socecon_vars_by_race <- mydata %>% select("num_educ","married", "primary", "secondary", "tertiary") %>% group_by(mydata$race) %>% na.omit %>% summarise_all(funs(mean)) %>% as.data.frame()
socecon_vars_by_race <- round(socecon_vars_by_race[2:6], digits = 2)
socecon_vars_by_race <- cbind(col_1,socecon_vars_by_race) 
colnames(socecon_vars_by_race) <- c("Race","Av.Educ","Married","Primary","Secondary","Tertiary")
rm(col_1)
print(socecon_vars_by_race)

#Regression Analysis
linear.1 <- lm(dbwt ~ asian + mexican + black, data = mydata)
linear.2 <- lm(dbwt ~ white + mexican +asian + black - 1, data = mydata )

#Seasonality Analysis
mydata$month <- factor(mydata$month, labels = c("January","February","March","April","May","June","July","August","September","October","November","December"), levels = c(1,2,3,4,5,6,7,8,9,10,11,12))

linear.3 <- lm(dbwt ~ asian + mexican + black + month, data = mydata)

tab.1 <- stargazer(linear.1,linear.2, linear.3 , title = "Regression Results", type = "text", omit.stat = c("LL","AIC", "BIC"), dep.var.labels = "Mean Infant Weight")

#i)
print("The constant term measures rought weight of a child from a white mother. Use tapply(mydata$dbwt,list(mydata$white == 1), FUN = mean) to verify")

#ii)
print("Lowest infants are born in December while the heaviest are born in June.")


#Bonus
gr_1 <- subset(mydata$dbwt, mydata$month == "December")
gr_2 <- subset(mydata$dbwt, mydata$month == "June")

test.1 <- t.test(gr_2, gr_1, alternative = "two.sided", conf.level = 0.95, var.equal = FALSE) 
if (test.1$p.value > 0.05) 
{print("The difference in means is not statistically significant")} else {"The difference in means is statistically significant"}
rm(gr_1,gr_2)

#iii)
print("R squared is a measure of goodness of  fit of regression line that minimizes the sum of residual squares. If R squared is low - there is a lot of unexplained variation in the data, if it is high - our best fit regression line captures well the variation in the data set")

#3 Black - White racial gap
mydata <- subset(mydata, mydata$black == 1 | mydata$white == 1)


#a)
rm(linear.1,linear.2,linear.3)
linear.1 <- lm(dbwt ~ black, data = mydata)
linear.2 <- lm(dbwt ~ black + mager + secondary + tertiary, data = mydata)

#b)
linear.3 <- lm(dbwt ~ black + secondary +tertiary + (black * secondary) + (black * tertiary), data = mydata)

#c)
linear.4 <- lm(dbwt ~ black + mager + secondary + tertiary + smoke + prenatal, data = mydata)
linear.5 <- lm(dbwt ~ black + mager + secondary + tertiary + smoke + prenatal + black + (black * secondary) + (black * tertiary), data = mydata)

tab.2 <-  stargazer(linear.1,linear.2, linear.3, linear.4,linear.5 , title = "Regression Results", type = "text", omit.stat = c("LL","AIC", "BIC"))

print("WE clearly see the difference between child birth weight between blacks and whites. Onces we control for education - the difference decreases. The interaction term captures the simultaneous impact of two factors on the dependent variabe in a non-additive fashion. Hence, it is the joint impact on dbwt by the X_i : black individuals with secondary and tertiary(x1,x2) levels of education. The point estimates of the racial birth gap are significantly different.")

#d) Auxiliary regression

linear.1.aux <- lm(black ~ mager + secondary + tertiary + smoke + prenatal, data = mydata)
predicted_values <- predict.lm(linear.1.aux)
r1 <- mydata$black - predicted_values
linear.2.aux <- lm(mydata$dbwt ~ r1)
tab.3 <- stargazer(linear.4, linear.2.aux, type = "text")
