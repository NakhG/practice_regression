#Ch. 6, Variable Screening Methods
#Question 6.6, pg. 340
#Clerical staff work hours

#dataset CLERICAL

#CLERICAL <- load("~/CLERICAL.Rdata")
head(CLERICAL)

#a
#Conduct a stepwise regression

#stepwise
#first we need a linear model
clerical.model.init <- lm(Y~X1+X2+X3+X4+X5+X6+X7, data=CLERICAL)
#now we do the stepwise
model = step(clerical.model.init, direction="both")
model
summary(model)
