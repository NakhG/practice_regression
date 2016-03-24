#Ch. 7, Variable Transformations
#Question 7.20, pg. 381
#Log-log transformation

library(ggplot2)


#Uses dataset 'EX7_20'
#EX7_20 <- load("~/EX7_20.Rdata")

head(EX7_20)

#Prompt: gives a table of data w/ two variables: x, y

#a
#Plot the points on a scatterplot. What type of relationship appears to exist between x and y?
plot(X, Y, data=EX7_20)

ggplot(data=EX7_20, aes(x=X, y=Y)) + geom_point(size=3) + labs(title="EX7_20 overview")

#b
#For each observation calculate ln x and ln y. Plot the log-transformed data points on a scatterplot. What type of relationship appears to exist between ln x and ln y?

#lets transform variables
EX7_20$lnX <- log(EX7_20$X)
EX7_20$lnY <- log(EX7_20$Y)

ggplot(data=EX7_20, aes(x=lnX, y=lnY)) + geom_point(size=3) + labs(title="EX7_20 overview: transformed variables")
#looks linear now

#c
#The scatterplot from part b suggests that the transformed model ln y = b0 + b1 ln x + e may be appropriate. Fit the transformed model to the data. Is the model adequate? Test using alpha = .05.

#lets try a linear model w/ the new ln variables
model <- lm(lnY ~ lnX, data=EX7_20)
summary(model)

#use the model to predict the value of y when x=30

#so lets grab the betas and pay attention to what our exact model is:
# lnY = 10.6364 - 2.169(lnX)
#so if x=30: lny = 10.6364 - 2.169 (ln(30))

#practice using the predict function instead of just recreating the model manually
new_data <- data.frame(lnX = log(30))
predict(model, newdata=new_data)

#check against manual
prediction <- 10.6364 - (2.169*log(30))
#so that worked, cool

#this result is for lnY, though, and we need to convert to Y terms, so we multiple by e


#d
#Use the transformed model to predict the value of y when x = 30. [Hint: Use the inverse transformation y = eln y]
exp(prediction)
