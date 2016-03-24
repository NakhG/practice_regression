#Chapter 9, Logistic Regression
#Question 9.25, pg. 504
#Gender discrimination in hiring

#uses DISCRIM dataset

#DISCRIM <- load("~/DISCRIM.Rdata")
head(DISCRIM)

#a
#Conduct a test of model adequacy. Use alpha = .05
#we want to make a model
fit <- glm(HIRE ~ EDUC + EXP + GENDER, data=DISCRIM, family = binomial(logit))
summary(fit)
anova(fit, update(fit, ~1), test="Chisq")

#c
# find a 95% confidence interval for the mean response when x1=4, x2=0 and x3=1

pred <- predict(fit, data.frame(EDUC = 4, EXP = 0, GENDER=1), type="link", se.fit=TRUE)
critval = 1.96
logit.point = pred$fit
logit.lower = pred$fit - (critval * pred$se.fit)
logit.upper = pred$fit + (critval * pred$se.fit)
logit.point; logit.lower; logit.upper

prob.point = exp(logit.point)/(1+exp(logit.point))
prob.lower = exp(logit.lower)/(1+exp(logit.lower))
prob.upper = exp(logit.upper)/(1+exp(logit.upper))
prob.point; prob.lower; prob.upper