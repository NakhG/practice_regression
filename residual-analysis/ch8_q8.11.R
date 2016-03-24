#Ch. 8, Residual Analysis
#Question 8.11, pg. 407
#Cooling method for gas turbines

#uses GASTURBINE dataset
#GASTURBINE <- load("~/GASTURBINE.Rdata")
head(GASTURBINE)

#Consider an interaction model that predicts heatrate using cycle speed (x1) and cycle pressure reatire (x2)
#fit that model

interaction_model <- lm(HEATRATE ~ RPM * CPRATIO, data=GASTURBINE)
summary(interaction_model)

GASTURBINE$residuals <- residuals(interaction_model)
predict.lm(interaction_model)
class(predict.lm(interaction_model))

GASTURBINE$HEATRATE_predicted <- predict.lm(interaction_model)
head(GASTURBINE)

ggplot(data=GASTURBINE, aes(x=HEATRATE_predicted, y=residuals)) + geom_point() + geom_line(y=0, color="red", linetype=2)

#so we see there's a pretty big amount of difference in residuals

#is there a more formal test for heterscedasticity?
#apparently so: 

bptest(interaction_model)
