#Chapter 5, Model Building
#Question 5.19, pg. 281
#Cooling method for gas turbines

##Consider a model for heat rate (kilojoules per kilowatt per hour) of a gas turbine 
#as a function of cycle speed (revolutions per minute) and cycle pressure ratio

#uses dataset GASTURBINE
#GASTURBINE <- load("~/GASTURBINE.Rdata")

#where is the polynomial relationship?
ggplot(data=GASTURBINE, aes(x=RPM, y=HEATRATE)) + geom_point(size=2)
ggplot(data=GASTURBINE, aes(x=CPRATIO, y=HEATRATE)) + geom_point(size=2)

#looks like both could require a quadratic model
#is there interaction between the two predictors?
ggplot(data=GASTURBINE, aes(x=RPM, y=CPRATIO)) + geom_point(size=2)

cor.test(x=GASTURBINE$RPM, y=GASTURBINE$CPRATIO)
#correlation is -0.5, and statistically signf. ..so can't ignore it

GASTURBINE$RPM.sq <- GASTURBINE$RPM**2
GASTURBINE$CPRATIO.sq <- GASTURBINE$CPRATIO**2

 quadratic.interaction.model <- lm(HEATRATE ~ RPM*CPRATIO + RPM.sq + CPRATIO.sq, data=GASTURBINE)
summary(quadratic.interaction.model)

#y=15580 + .078(RPM) -523(CPRATIO)-0.0000002(RPM^2)+8.84(CPRATIO^2)+0.0045(RPM*CPRATIO)

reduced.interaction.model <-lm(HEATRATE ~ RPM*CPRATIO, data=GASTURBINE)
summary(reduced.interaction.model)

anova(quadratic.interaction.model, reduced.interaction.model)


#part d
#graph the prediction model

x1r = range(GASTURBINE$RPM)
RPM = seq(x1r[1], x1r[2], length=13)  # 13 obs
x2r = range(GASTURBINE$CPRATIO)
CPRATIO = seq(x2r[1], x2r[2], length=13)  # 13 obs
HEATRATE = outer(RPM, CPRATIO, function(a, b) predict(quadratic.interaction.model, newdata = data.frame(RPM=a, CPRATIO=b, RPM.CPRATIO = a*b, RPM.sq = a^2, CPRATIO.sq = b^2)))
res=persp(x=RPM, y=CPRATIO, z=HEATRATE)
mypoints = trans3d(GASTURBINE$RPM,GASTURBINE$CPRATIO,GASTURBINE$HEATRATE, pmat=res)
points(mypoints, pch=1, lwd=2, col="red")

#y=15580+.078(RPM)-523(CPRATIO)-0.0000002(RPM^2)+8.84(CPRATIO^2)+0.0045(RPM*CPRATIO)
#y=15580 + .078(RPM) -523(CPRATIO)-0.0000002(RPM^2)+8.84(CPRATIO^2)+0.0045(RPM*CPRATIO)
prediction.line1 <- 15580 + .078*(5000) -523*(GASTURBINE$CPRATIO)-0.0000002*(5000**2)+8.84*(GASTURBINE$CPRATIO.sq)+0.0045*(5000*GASTURBINE$CPRATIO)
plot(x=GASTURBINE$CPRATIO, y=prediction.line1)

prediction.line2 <- 15580 + .078*(15000) -523*(GASTURBINE$CPRATIO)-0.0000002*(15000**2)+8.84*(GASTURBINE$CPRATIO.sq)+0.0045*(15000*GASTURBINE$CPRATIO)
plot(x=GASTURBINE$CPRATIO, y=prediction.line2)

