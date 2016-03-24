#Ch. 7, Variable Transformations
#Question 7.21, pg. 381
#Multicollinearity in real estate data

library(ggplot2)

#Uses HAMILTON dataset
#HAMILTON <- load("~/HAMILTON.Rdata")
head(HAMILTON)
#x is land value, x2 is appraised improvement value, y is sale price

#a 
#Calculate the coefficient of correlation between y and x1. Is there evidence of a linear relationship between sale price and appraised land value?

cor(HAMILTON)
#correlation between y and x1 is 0.0025

ggplot(data=HAMILTON, aes(x=X1, y=Y)) + geom_point(size=3)
#no evidence of a linear relationship, look at that shotgun blast ;)

#b
#Calculate the coefficient of correlation between y and x2. Is there evidence of a linear relationship between sale price and appraised improvements?
#we did that before: it's 0.434

ggplot(data=HAMILTON, aes(x=X2, y=Y)) + geom_point(size=3)
#in between... mild correlation

#c
#Based on the results in parts a and b, do you think the model E(y) = B0 + B1x1 + B2x2 will be useful for predicting sale price?
#I don't think it would be very good, no

#d
#Use a statistical computer software package to fit the model in part c, and conduct a test of model adequacy. In particular, note the value of R2. Does the result agree with your answer to part c?

model <- lm(Y ~ X1+X2, data=HAMILTON)
summary(model)

#unexpected: the model is super, super strong!

#e
#Calculate the coefficient of correlation between x1 and x2. What does the result imply?

#lets look at the correlation of coefficient of x1 and x2

cor(HAMILTON)
#really strong: -0.899
#implies there is multicollinearity: that the value of the land has a strong negative with value of improvement value
ggplot(data=HAMILTON, aes(x=X1, y=X2)) + geom_point(size=3)


#f
#would you recommend the common approach of dropping a predictor to sidestep the issue of multicollinearity?

#well we can't: the model is useless without the two combined
#take a step back: we can think about the relationship between land value, improvement value, and sales price
#land and improvement are both not great ways of describing a house, and we're trying to predict the sales price
#however: combine the two, and you have a something more resembling a house

#not sure why you wouldn't want to add an interaction term, though



