#Chapter 5, Model Building
#Question 5.20, pg. 287
#Tire wear and pressure

#uses TIRES2 dataset
#TIRES2 <- load("~/TIRES2.Rdata")
head(TIRES2)

#Give the equation relating the coded variable u to pressure, x, 
#using the coding system for observational data.

#the equation is u = (x - xbar) / std

#Use scale() function

TIRES2$PressureZ = scale(TIRES2$X_PSI)
head(TIRES2)

#lets do by hand to get practice
xbar = mean(TIRES2$X_PSI)
stdev = sd(TIRES2$X_PSI)
TIRES2$U = (TIRES2$X_PSI - xbar)/stdev
head(TIRES2)   #OK super

#calculate correlation between x and xsquare
cor.before = cor(TIRES2$X_PSI, (TIRES2$X_PSI**2))
cor.before

cor.after = cor(TIRES2$U, (TIRES2$U**2))
cor.after

TIRES2$U.sq = TIRES2$U**2
fit.u <- lm(TIRES2$Y_THOUS ~ TIRES2$U + TIRES2$U.sq)
summary(fit.u)
