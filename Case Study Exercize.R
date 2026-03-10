#John Woobriddge 

##############
##Objectives##
##############

#The objective of this Case study excise is determine what relationship does an undergraduate GPA, GRE/GMAT Scores
#and the prestige of an applicant's undergraduate program effect admission into graduate school 
#
#This assignment consists of: 
#1) understanding the data set
#2) Determining any significant relationship between variables 
#3) Making recommendations for what actions should be taken 
setwd("~/Employment/Work Case Study")
getwd()
.libPaths()
.libPaths("C:/Program Files/R/R-3.5.2/library")
.libPaths()
# First step is to read in the data file and get the relevant libraries
install.packages("effects")
library(effects)
library(readxl)
library(ggplot2)
library(dbplyr)
library(xlsx)
library(aod)


library(readxl)
Case_Study_Dataset <- read_excel("Case Study Dataset.xlsx")
View(Case_Study_Dataset)
head(Case_Study_Dataset)

#Summary Statistics
summary(Case_Study_Dataset)
#Utilizing the summary function gives us the minimum, maximum, mean median and 1st and 3rd quartiles for each of the variable
#observations. We can use separate functions for each of the variables. The variables of interest are the non-binary variables
# GPA and GRE. Range gives us our Min and Max of the variable. Mean the Average. Median the 50th percentile. Quantile in increments of 
# 25 percentiles. sd the standard deviation  

range(Case_Study_Dataset$GRE)
mean(Case_Study_Dataset$GRE)
median(Case_Study_Dataset$GRE)
quantile(Case_Study_Dataset$GRE)
sd(Case_Study_Dataset$GRE)

range(Case_Study_Dataset$GPA)
mean(Case_Study_Dataset$GPA)
median(Case_Study_Dataset$GPA)
quantile(Case_Study_Dataset$GPA)
sd(Case_Study_Dataset$GPA)

#Correlation Matrix
#We can create a correlation matrix to see if there is any correlation between the different variables 
cor(Case_Study_Dataset)

#I'm further interested in the correlation between GRE and GPA our two numeric variables so we can plot this out:
attach(Case_Study_Dataset)
plot(GPA, GRE, main="GRE and GPA Scatterplot",
     xlab="GPA", ylab="GRE Score", pch=19)
abline(lm(GRE~GPA), col="red") # regression line (y~x)
lines(lowess(GPA,GRE), col="blue") # lowess line (x,y)
#Converting to factors
#Since whether the school is top notch or not and ADMIT are binary variables we have to convert both into factors so that it will be treated 
# as a categorical variable, in essence we are transforming the TOPNOTCH variable into a dummy variable  
Case_Study_Dataset$TOPNOTCH <- factor(Case_Study_Dataset$TOPNOTCH)
Case_Study_Dataset$ADMIT <- factor(Case_Study_Dataset$ADMIT)
#Logistic Regression GLM 
#Now after doing all our steps here is the part where we actually create our model to answer the question of determining the likelihood that a student
#will be admitted. Since our dependent variable is a binary outcome (they are either admitted or not admitted), the appropriate regression to use would be a logistic regression
#as opposed to a standard linear regression. .... (see attached write up doc)
logitreg <- glm(ADMIT ~ GRE + GPA + TOPNOTCH, data = Case_Study_Dataset, family = "binomial")
summary(logitreg)
plot(allEffects(logitreg))


#checking the confidence intervals 
confint(logitreg)

#Wald Test
#From the results of our model we can see that GRE & GPA are statistically significant, however the TOPNOTCH variable is not, what we can do is conduct a wald-test to 
#examine if dropping the statistically insignificant variable will improve the accuracy of our model! 
wald.test(b=coef(logitreg), Sigma = vcov(logitreg), Terms = 4:4)

#^the chi-squared test statistic of 2.2 to the 1st degree of freedom with a p-value of .13 indicating that the overall effect of TOPNOTCH is not significant. Thus we can try a second
#logit model dropping the TOPNOTCH variable and regressing GRE and GPA on admittance


#pseudo-r-squared
#In a normal multi-linear regression we use r-squared as a measure of statistical power of our model (how much of our data set can be explained by our model, in essence what is the "goodness of fit" for example an R^2 of .3 that 30% can be explained by our model)
#however for a logistic regression an equivalent statistic to R-squared does not exist so a metric called pseudo r-squared is used.
ll.null <- logitreg$null.deviance/-2
ll.proposed <- logitreg$deviance/-2
(ll.null-ll.proposed)/ll.null


#Our pseudo r-squared is .04369585 meaning that our model has very low predictive power

#Odds Ratio 
#Another way to interpret the results of our model  
exp(coef(logitreg))
#Interpretation: For 1 unit increase in GPA of 1.0 points the odds of being admitted to grad school increase by a factor 1.95.


#Plotting
#In addition we can plot a graph of our model, we're looking at what our logistic regression predicts  
predicted.data <- data.frame(probability.of.admit=logitreg$fitted.values,ADMIT=Case_Study_Dataset$ADMIT)

predicted.data <- predicted.data[order(predicted.data$probability.of.admit, decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.admit)) +
  geom_point(aes(color=ADMIT), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("proability of admitence")

#Interpretation
#The plot created ranks our 400 observations from most to least likely to be admitted with our x-axis being the ranking and y-axis being the predicted probability 
#The salmon colored x's (0) are students that were not admitted while the blue colored x's (1) are students that were admitted.
#


