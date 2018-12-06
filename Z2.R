
#Task no.2 
library(psych) # for describe
library(dplyr) # for data management
library(gsheet) # to read data from google sheets
library(ggplot2) # for ggplot
library(car)
library(lm.beta)
library(lsr) 
library(tidyverse)
library(caret)
library(leaps)
library("MASS", lib.loc="~/R/win-library/3.5")
library(MASS)

rm(list=ls(all=TRUE)) # clears the workspace
graphics.off() # clears graphics


# dataloading
data_pain = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")
data_pain2 <- data_pain[-c(28,146,112), ] 
levels(data_pain2$sex) <- c(1,2)
View(data_pain2)
format.data.frame(data_pain2)

# regression 
mod_pain1 <- lm(pain ~ sex + age, data = data_pain2)
mod_pain3 <- lm(pain ~ pain_cat + sex + age + STAI_trait  + cortisol_serum + mindfulness, data = data_pain2)


# Fit the full model 
full.model <-  lm(pain ~ pain_cat + sex + age + STAI_trait  + cortisol_serum + mindfulness + weight, data = data_pain2)

# Stepwise regression model
step.model <- step(full.model, direction = "backward")
summary(step.model)
lm.beta(step.model)
round(confint(step.model),2)

#assumptions
ncvTest(full.model) #check 0.88
cooks.distance(full.model)
vif(full.model) #check, between 1.02 to 1.92

plot(full.model, 1) # linearity check, outliers 24,47,100

residualPlots( model = full.model )#check linearity , turkey test is not significant, nice for all variables

shapiro.test(full.model$residuals) #normality check .79

#new model after backwards regression without weight and STAI trait

new.model <- lm(pain ~ pain_cat + sex + age  + cortisol_serum + mindfulness, data = data_pain2)
summary(new.model) #exactly same adj. r2

ncvTest(new.model) #check 0.79
cooks.distance(new.model)
vif(new.model) #check, between 1.01 to 1.48

plot(new.model, 1) # linearity check, outliers 24,47,100

shapiro.test(new.model$residuals) #normality check .70

#compare the models
anova(new.model, mod_pain3) #not significant!!

AIC(mod_pain3)

AIC(new.model)

data_pain_new = read.csv( "https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv" )
levels(data_pain_new$sex) <- c(1,2)

# calculate predicted values 
pred_test1 <- predict(mod_pain3, data_pain_new) 
pred_test2 <- predict(new.model, data_pain_new)

# now we calculate the sum of squared residuals
RSS_test1 = sum((data_pain_new[, "pain"] - pred_test1)^2) 
RSS_test_back = sum((data_pain_new[, "pain"] - pred_test2)^2)
RSS_test1 # 245.04
RSS_test_back #247.03

#really small difference

