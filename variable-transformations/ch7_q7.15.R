#Ch. 7, Variable Transformations
#Question 7.15, pg. 380
#Collusive bidding in road construction. 

#uses FLAG2 dataset

#Dataset is related to investigation of bid-rigging in road construction industry

#we are going to look at the variables chosen from stepwise regression
#and we're going to test for multicollinearity
#if we find that going on, what variables do we recommend?

#so lets look at the data
#FLAG2 <- load("~/FLAG2.Rdata")
head(FLAG2)
library(MASS)

#lets do the stepwise regression
full.model <- lm(LOWBID ~ DOTEST+LBERATIO+STATUS+DISTRICT+NUMBIDS+DAYSEST+RDLNGTH+PCTASPH+PCTBASE+PCTEXCAV + PCTMOBIL + PCTSTRUC + PCTTRAFF + SUBCONT, data=FLAG2)
model = step(full.model, direction="both")

#lets test for multicollinearity by looking at correlation: 
#what variables are highly correlated to one another?
cor(FLAG2)
#looks like LOWBID:DOTEST:DAYSEST are correlated to one another

#another way to test: variance inflation factor
#theres a few packages we need to get to vif

install.packages("car") # one time only, required for vif
install.packages("minqa")
vif(full.model)

#do we even remember how to interpret variance inflation vactor?!
# vif = 1, not correlated ...  vif 1-5, mildly correlated, vif 5-10 highly correlated

#PCTASPH has a wacky variance
