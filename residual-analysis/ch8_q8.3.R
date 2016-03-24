#Ch. 8, Residual Analysis
#Question 8.3, pg. 396
#New tire wear tests

library(ggplot2)

#uses the TIRES dataset
#TIRES <- load("~/TIRES.Rdata")
head(TIRES)

#a
#Fit the straight-line model y = B0 + B1x + e to the data
model <- lm(MILEAGE_Y ~ PRESS_X, data=TIRES)

#b
#Calculate the residuals for the model
residuals(model)

#c
#Plot the residuals versus x. Do you detect any trends? If so, what does the pattern suggest about the model?
#how do we graph residuals?
TIRES$residuals <- residuals(model)

ggplot(data=TIRES, aes(x=PRESS_X, y=residuals)) + geom_point(size=2) +
  labs(title="Residuals from the Tires model")

#d
#Fit the quadratic model y = ??0 + ??1x + ??2x2 + ?? to the data using an available statistical software package. Has the addition of the quadratic term improved model adequacy?

TIRES$PRESS_X_sq <- TIRES$PRESS_X^2
quad_model <- lm(MILEAGE_Y ~ PRESS_X + PRESS_X_sq, data=TIRES)

ggplot(data=TIRES, aes(x=PRESS_X, y=MILEAGE_Y)) + geom_point(size=2) +
  geom_smooth(method="lm")

ggplot(data=TIRES, aes(x=PRESS_X, y=MILEAGE_Y)) + geom_point(size=2) +
  geom_smooth(method="lm")

ggplot(data=TIRES, aes(x=PRESS_X, y=MILEAGE_Y)) + geom_point(size=2) +
  geom_smooth(method="lm", formula= y ~ x + I(x^2), color="red")