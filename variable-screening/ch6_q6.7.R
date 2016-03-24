#Ch. 6, Variable Screening Methods
#Question 6.7, pg. 341
#Clerical staff work hours

#dataset CLERICAL

#CLERICAL <- load("~/CLERICAL.Rdata")

#perform an all-possible-regressions procedure on the data w/ only x1 thru x4

library(leaps)
yvar = c("Y")
xvars = c("X1", "X2", "X3", "X4")
model.allpossible=leaps(x=CLERICAL[,xvars], y=CLERICAL[,yvar], names=xvars, method="Cp")
model.allpossible$which

#a: how many models for E(y) are possible? if the model includes
#1 variable, 2 variables, three variables, 4 variables



#b: for each case, use stats to find the max Rsquared, minimum MSE, min Cp and min PRESS
model.allpossible$Cp


model.allpossible.adjr=leaps(x=CLERICAL[,xvars], y=CLERICAL[,yvar], names=xvars, method="adjr2")
model.allpossible.adjr$adjr2
model.allpossible.r2=leaps(x=CLERICAL[,xvars], y=CLERICAL[,yvar], names=xvars, method="r2")
model.allpossible.r2$r2

which.min(model.allpossible.adjr$adjr2)
which.max(model.allpossible.r2$r2)

which.min(model.allpossible$Cp)
model.allpossible$which[5,]
install.packages("MPV")

head(CLERICAL)
