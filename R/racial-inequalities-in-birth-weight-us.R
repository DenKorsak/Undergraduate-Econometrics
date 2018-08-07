
#Set working directory
setwd("C:/Users/Denis Korsak/Desktop/Business Economics/Econometrics 4260/Assignments/Assignment 1")

#Download packages
library(foreign)
library(graphics)
library(stats)
library(readstata13)
library(ggplot2)
library(dplyr)

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
#categorical variables for each education level. #since R does not recognize a�� simbols

num_educ <- as.numeric(mydata$meduc)
table(num_educ)

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
mydata <- mydata[c("month","race","white","black","mexican","asian","married", "prenatal", "cig_0", "dbwt", "boy", "educ", "smoke", "smoker", "primary", "secondary", "tertiary")]
class(mydata)


#h)
#save dataset in R format
save(mydata, file = "us_dbwt_2015.Rda")



#Descriptive Statistics

#a) 

child_weight_by_education <- with(mydata, tapply(mydata$dbwt, list(Education = mydata$educ),mean)) %>% print()
print("We do  find significant differences between three groups. In particular, the difference between primary and tertiary group is 139 gramms.")

#b)

child_weight_by_race <- with(mydata, tapply(mydata$dbwt, list(Race = mydata$race),mean)) %>% print()
print("Again, there is a significant difference in weight between race groups. In particular, the difference between Black and White kids is the largest")

#c)

child_weight_by_race_and_education <- with(mydata, tapply(mydata$dbwt, list(Race = mydata$race,  Education = mydata$educ),mean)) %>% print()
print("We find significant differences in childweight of each group excep Asians. The weight of a baby increases as the levele of education rises")


#d)

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



# Student test 
test_1 <- t.test(gr_1, gr_2, alternative = "two.sided", conf.level = 0.95, var.equal = FALSE)
if (test_1$p.value > 0.05) 
  {print("We cannot reject the null hypothesis that smoking is harmless at 5% significance level between non-smoker and smoker groups.")} else {"We reject the null hypothesis that smoking is harmless to infants birth weight at 5% level of significance between non-smoker and smoker groups."}

test_2 <- t.test(gr_1, gr_3, alternative = "two.sided", conf.level = 0.95, var.equal = FALSE )
if (test_2$p.value > 0.05) 
{print("We cannot reject the null hypothesis that smoking is harmless at 5% significance level between before only smokers and before and after smoker groups.")} else {"We reject the null hypothesis that smoking is harmless to infants birth weight at 5% level of significance between before only smokers and before and after smoker groups."}



