library(psych) # for describe
library(psycho)
library(ggplot2) # for ggplot 
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(lme4) # for lmer 
library(lmerTest) # to get singificance test in lmer
library(lm.beta)
library(tidyverse)
library(lsr) 
library(reshape2)
library(car)
library(influence.ME) # for influence (this will also load the lme4 package) 
library(lattice) # for qqmath

rm(list=ls(all=TRUE)) # clears the workspace
graphics.off() # clears graphics


# dataloading

data_pain_ex3 = read.csv ("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv") 
View(data_pain_ex3)

   #check my data
# descriptives
describe(data_pain_ex3)
summary(data_pain_ex3)

# histograms 

plot(data_pain_ex3$pain_cat, xlab="sample", ylab="pain_cat") #no outlies, looks good
plot(data_pain_ex3$cortisol_serum, xlab="sample", ylab="cortisol_serum")
plot(data_pain_ex3$cortisol_saliva, xlab="sample", ylab="cortisol_saliva")
plot(data_pain_ex3$age, xlab="sample", ylab="age") 
plot(data_pain_ex3$sex, xlab="gender", ylab="sample")

table(data_pain_ex3$sex) #irregularities in categorical variable #no
table(data_pain_ex3$age) #irregularities in categorical variable #no

# designate which are the repeated varibales 
repeated_variables = c("pain1", "pain2", "pain3", "pain4")

# correlation of repeated variables
cor(data_pain_ex3[, repeated_variables])

#change into long format
data_pain_long = melt(data_pain_ex3, measure.vars = repeated_variables, variable.name = "time", value.name = "pain_rating")


# order data frame by participant ID(not necessary, just # makes the dataframe look more intuitive) 
data_pain_long = data_pain_long[order(data_pain_long[, "ID"]), ]

# change the time variable to a numerical vector
data_pain_long$time = as.numeric(data_pain_long$time)
View(data_pain_long)

mod_rep_int = lmer(pain_rating ~ time + cortisol_serum + pain_cat + mindfulness + STAI_trait + sex + age + (1 | ID), data = data_pain_long)
mod_rep_slope = lmer(pain_rating ~ time + cortisol_serum + pain_cat + mindfulness + STAI_trait + sex + age + (time | ID), data = data_pain_long)

#compare models
cAIC(mod_rep_int)$caic #same
cAIC(mod_rep_slope)$caic #same
anova1 <- anova(mod_rep_int,mod_rep_slope)
anova1

#nice summary for rep_int_model
results2 <- analyze(mod_rep_int, CI = 95)

summary(results2)
mutate(p = psycho::format_p(p))

print(results2)
#summary for rep_slope_model
results3 <- analyze(mod_rep_slope, CI = 95)

summary(results3)
mutate(p = psycho::format_p(p))

print(results3) 

#Prediction by model
data_pain_long_withpreds = data_pain_long 
data_pain_long_withpreds$pred_int = predict(mod_rep_int) 
data_pain_long_withpreds$pred_slope = predict(mod_rep_slope)


#graphic slope
ggplot(data_pain_long_withpreds, 
       aes(y = pain_rating, x = time, group = ID)) + 
  geom_point(size = 3) + geom_line(color = "red", 
      aes(y = pred_slope, x = time)) + facet_wrap(~ID, ncol = 5)

#does not really fit for participant 11,13,16
#graphic int
ggplot(data_pain_long_withpreds, 
       aes(y = pain_rating, x = time, group = ID)) + 
  geom_point(size = 3) + geom_line(color = "red", 
        aes(y = pred_int, x = time)) + facet_wrap(~ID, ncol = 5)

#critical for ID 11,12,3,20


mod_rep_slope_quad = lmer(pain_rating ~ time + cortisol_serum + I(time^2) + pain_cat + mindfulness + STAI_trait + sex + age + (time | ID), data = data_pain_long)
#predict ratings
data_pain_long_withpreds$pred_slope_quad = predict(mod_rep_slope_quad)

data_pain_long_withpreds$pred_slope_quad = predict(mod_rep_slope_quad)

#graphic square time
 ggplot(data_pain_long_withpreds, aes(y = pain_rating, x = time, group = ID)) + 
  geom_point(size = 3) + geom_line(color = "red", aes(y = pred_slope_quad, x = time)) + facet_wrap(~ID, ncol = 5) 

#overview
summary(mod_rep_slope_quad)
summary(mod_rep_slope)


 #center time
data_pain_long_centered_time = data_pain_long 
data_pain_long_centered_time$time_centered = data_pain_long_centered_time$time - mean(data_pain_long_centered_time$time)
mod_rep_slope_quad = lmer(pain_rating ~ time_centered + I(time_centered^2) + cortisol_serum +sex + age + mindfulness + STAI_trait + pain_cat + (time | ID), data = data_pain_long_centered_time) 
 #new model with centered time 


 #compare again 
 cAIC(mod_rep_slope_quad)$caic
 
 #R-square value
 r2beta(mod_rep_slope_quad, method= "nsj", data= data_pain_long_centered_time)
 
 cAIC(mod_rep_slope_quad)$caic
 
 #confidence interval
 round(confint(mod_rep_slope_quad),2)
 summary(mod_rep_slope_quad)
 
 #nice summary for table
 results <- analyze(mod_rep_slope_quad, CI = 95)
 
 summary(results)
   mutate(p = psycho::format_p(p))
   
   print(results)
   
  
   
data_pain_long_with_resid = data_pain_long_centered_time 
data_pain_long_with_resid$resid = residuals(mod_rep_slope_quad)
   
#influential outliers
influence_observation = influence(mod_rep_slope_quad, obs = T)$alt.fixed 
influence_group = influence(mod_rep_slope_quad, group = "ID")$alt.fixed

boxplot(influence_observation[, "time_centered"])
#loop for influential observation

pred_names = colnames(influence_group)
par(mfrow = c(1, length(pred_names)))
for (i in 1:length(pred_names)) {
   boxplot(influence_observation[, pred_names[i]], main = pred_names[i])
  
}

dev.off()
#normality
qqmath(mod_rep_slope_quad, id = 0.05)
qqmath(ranef(mod_rep_slope_quad))

#linearity
attach(mtcars)
par(mfrow=c(2,4))

plot(resid ~ time_centered, data = data_pain_long_with_resid)
plot(resid ~ time_centered_2, data = data_pain_long_with_resid)
plot(resid ~ sex, data = data_pain_long_with_resid)
plot(resid ~ age, data = data_pain_long_with_resid)
plot(resid ~ mindfulness, data = data_pain_long_with_resid)
plot(resid ~ cortisol_serum, data = data_pain_long_with_resid)
plot(resid ~ STAI_trait, data = data_pain_long_with_resid)
plot(resid ~ pain_cat, data = data_pain_long_with_resid)


## asign ID and location as factors
data_pain_long_centered_time$ID = factor(data_pain_long_centered_time$ID) 
#new vaiable square time
data_pain_long_centered_time$time_centered_2 = data_pain_long_centered_time$time_centered^2


#homoscedascity
homosced_mod = lm(data_pain_long_with_resid$resid^2 ~ data_pain_long_with_resid$ID) 

summary(homosced_mod) #not significant

# caluclate interquartile range within each cluster
IQR_of_residuals_by_participant = sapply(split(data_pain_long_with_resid, 
         f = data_pain_long_with_resid$ID), function(x)
               IQR(x$resid)) # rank ordering them 

rank = rank(IQR_of_residuals_by_participant) # adding rank to the dataframe containing the residuals
data_pain_long_with_resid$rank = rep(rank, each = length(repeated_variables)) # creating a vector of participant IDs ordered based on the # rank, this will be used as labels 
IDforplot = unique(data_pain_long_with_resid$ID[order(data_pain_long_with_resid$rank)])

# create the plot 
ggplot(data_pain_long_with_resid, aes(y = resid, x = factor(rank),
             labels = ID)) + geom_boxplot() + scale_x_discrete(labels = IDforplot) + coord_flip()

#multicolinearity
pairs.panels(data_pain_long_centered_time[, c("time_centered", "time_centered_2", 
                                               "age", "sex", "mindfulness" , "pain_cat", "cortisol_serum", "STAI_trait")], col = "red", lm = T)
