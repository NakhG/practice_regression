#Chapter 9, Logistic Regression
#Question 9.24, pg. 503
#Flight response of geese

#Study to investigate the impact helicopter overflights have on the flocks of Pacific brant geese

library(dplyr)

#uses dataset PACGEESE
#goose_data <- load("~/PACGEESE.Rdata")
head(goose_data)

goose_data$response_dummy <- ifelse(goose_data$RESPONSE=="HIGH", 1, 0)
head(goose_data)

#b 
#do a logistic regression using only altitude to predict response

goose_data <- arrange(goose_data, ALTITUDE)

#plot
plot(response_dummy ~ ALTITUDE, data=goose_data, lwd=2) #doesn't look promising

#make our model
fit = glm(response_dummy ~ ALTITUDE, data=goose_data, family=binomial(logit))
summary(fit)

#how do we test if this is a good model?
#we can do a 'likelihood ratio test' which is like an F test

anova(fit, update(fit, ~1), test="Chisq")

#results in a PR of 0.0096, less than 0.01


#c 
#Conduct a test to determine if flight response of the geese depends on lateral distance of helicopter from the flock
#test using alpha = 0.01

#same thing!

goose_data <- arrange(goose_data, LATERAL)

#plot
plot(response_dummy ~ LATERAL, data=goose_data, lwd=2) #looks better

#make our model
fit2 = glm(response_dummy ~ LATERAL, data=goose_data, family=binomial(logit))
summary(fit2)

#how do we test if this is a good model?
#we can do a 'likelihood ratio test' which is like an F test

anova(fit2, update(fit2, ~1), test="Chisq")



#d
#predict the probability of high flight response from geese if:
#flying over at altitude of 6 hundred m and lateral distance of 3 hundred m

fit3 = glm(response_dummy ~ ALTITUDE + LATERAL, data=goose_data, family=binomial(logit))
summary(fit3)

#predict values [logits]
pred <- predict(fit3, data.frame(ALTITUDE = 6, LATERAL = 3))
pred

#convert to probability
prob <- exp(pred)/(1+exp(pred))
