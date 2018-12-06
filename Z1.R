

###########################################################

#                                                         #

#                 Loading packages                        #

#                                                         #

###########################################################
library(psych) # for describe
library(dplyr) # for data management
library(gsheet) # to read data from google sheets
library(ggplot2) # for ggplot
library(car)
library(lm.beta)
library(lsr) 

install.packages("Hmisc")
library("Hmisc")

###########################################################

#                                                         #

#                     Data management                     #

#                                                         #

###########################################################
rm(list=ls(all=TRUE)) # clears the workspace
graphics.off() # clears graphics


# dataloading
data_pain = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")


# compute some descriptive statistics #
summary (data_pain)


#look for outliers
plot(data_pain$pain_cat, xlab="sample", ylab="pain_cat") #no outlies, looks good
plot(data_pain$cortisol_serum, xlab="sample", ylab="cortisol_serum")
plot(data_pain$cortisol_saliva, xlab="sample", ylab="cortisol_saliva")
plot(data_pain$age, xlab="sample", ylab="age") # oone big outlier!!
plot(data_pain$sex, xlab="gender", ylab="sample")
table(data_pain$sex) #irregularities in categorical variable
table(data_pain$age) #irregularities in categorical variable 


#slightly more woman
# remove outliers
data_pain2 <- data_pain[-c(28,146,112), ] 
format.data.frame(data_pain2)
#Categorical into numeric
levels(data_pain2$sex) <- c(1,2)
View(data_pain2)

#check further
plot(data_pain2$mindfulness, xlab="sample", ylab="pain_cat") # score under 0??# see data_pain2 
plot(data_pain2$STAI_trait, xlab="sample", ylab="STAI_trait")


###########################################################

#                                                         #

#                 Hierarchical regression                 #

#                                                         #

###########################################################





mod_pain1 <- lm(pain ~ sex + age, data = data_pain2)

mod_pain2 <- lm(pain ~ pain_cat + sex + age + STAI_trait  + cortisol_serum +cortisol_saliva+  mindfulness, data = data_pain2)


#check for correlation & multicollinearity
data_pain21 <- data_pain2[,(-1)] #create matrix without ID and Sex, cause I dont need them
data_pain22 <- data_pain21[,(-2)]
round(cor(data_pain22),2) #highly correlation between 
cor(data_pain2$cortisol_saliva, data_pain2$cortisol_serum)

#assumption no excess multicollinearity ("uncorrelated predictors")
round(vif(mod_pain1),2)
round(vif(mod_pain2),2) #! cortisol saliva and cortisol serum are not good

mod_pain3 <- lm(pain ~ pain_cat + sex + age + STAI_trait  + cortisol_serum + mindfulness, data = data_pain2)


#cooks distance
cook <- cooks.distance(mod_pain1) 
cook3 = cooks.distance(mod_pain3)
summary(cook)
summary(cook3)
plot(cook,ylab="Cooks distances", xlab="obs. number") 
plot(cook3,ylab="Cooks distances", xlab="obs.number")


plot(x=mod_pain1, which= 4)  #35, 44 and 86
plot(x=mod_pain1, which= 5)

data_pain3 <- data_pain2[-c(35,44,86), ] 
format.data.frame(data_pain2)
mod_pain11 <- lm(pain ~ sex + age, data = data_pain3)
summary(mod_pain11)

plot(x=mod_pain3, which= 4)  #19,44,51
plot(x=mod_pain3, which= 5)
data_pain4 <- data_pain2[-c(19,44,51), ]
mod_pain31 <- lm(pain ~ pain_cat + sex + age + STAI_trait  + cortisol_serum + mindfulness, data = data_pain4)
summary(mod_pain31)

#assumption normality  (of the residuals)  

plot(mod_pain1,2)
plot(mod_pain3,2)
shapiro.test(mod_pain1$residuals) #not significant

shapiro.test(mod_pain3$residuals)# not significant

mean(mod_pain1$residuals) #nearly 0, thats good

#assumption linearity (of the relationship)
plot(mod_pain1, 1) #Ja

plot(mod_pain3, 1)

#assumption homogeneity of variance (also called homoscedasticity)
plot(mod_pain1, 3) 

plot(mod_pain3, 3)
ncvTest(mod_pain1) #not significant
ncvTest(mod_pain3) #not significant


#results models
summary(mod_pain1)
summary(mod_pain1)$adj.r.squared

summary(mod_pain3)
summary(mod_pain3)$adj.r.squared


# create table
round(confint(mod_pain1, level = .95),2)
round(confint(mod_pain3, level = .95),2)
lm.beta(mod_pain1)
lm.beta(mod_pain3)

# Test for difference
anova(mod_pain1, mod_pain3) #highly significant 

AIC(mod_pain1)

AIC(mod_pain3)

539.613 - 484.6589

