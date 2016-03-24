#Ch. 6, Variable Screening Methods
#Question 6.10, pg. 343
#Cooling method for gas turbines

#uses GASTURBINE dataset
#GASTURBINE <- load("~/GASTURBINE.Rdata")
head(GASTURBINE)
paste0(unique(GASTURBINE$ENGINE))

#dummify variables
GASTURBINE$Engine_Dummy1 <- ifelse(GASTURBINE$ENGINE == "Advanced   ", 1,0)
GASTURBINE$Engine_Dummy2 <- ifelse(GASTURBINE$ENGINE == "Aeroderiv  ", 1,0)

#a
#Use stepwise regression (with stepwise selection) to find the ''best'' predictors of heat rate
model.init <- lm(HEATRATE ~ Engine_Dummy1 + Engine_Dummy2 + RPM + CPRATIO + INLETTEMP + EXHTEMP + AIRFLOW + POWER + LHV + ISOWORK , data=GASTURBINE)
model = step(model.init, direction="both")
summary(model)
model

#b
#Use stepwise regression (with backward elimination) to find the ''best'' predictors of heat rate
model = step(model.init, direction="back")
summary(model)
model

#c
#Use all-possible-regressions-selection to find the ''best'' predictors of heat rate.
GASTURBINE.scrubbed <- na.omit(GASTURBINE)


yvar = c("HEATRATE")
xvars = c("SHAFTS" , "RPM" , "CPRATIO" , "INLETTEMP" , "EXHTEMP" , "AIRFLOW" , "POWER" , "LHV" , "ISOWORK", "Engine_Dummy1", "Engine_Dummy2")
model.allpossible=leaps(x=GASTURBINE.scrubbed[,xvars], y=GASTURBINE.scrubbed[,yvar], names=xvars, method="Cp")
model.allpossible$which

model.allpossible.adjr2=leaps(x=GASTURBINE.scrubbed[,xvars], y=GASTURBINE.scrubbed[,yvar], names=xvars, method="adjr2")
model.allpossible.r2 = leaps(x=GASTURBINE.scrubbed[,xvars], y=GASTURBINE.scrubbed[,yvar], names=xvars, method="r2")


which.min(model.allpossible.adjr$adjr2)
which.max(model.allpossible.r2$r2)

model.allpossible.r2$which[101,]

which.min(model.allpossible$Cp)
model.allpossible$which[61,]

which.min(model.allpossible.adjr$adjr2)
model.allpossible.r2$which[10,]

which.min(model.allpossible.r2$r2)
model.allpossible.adjr$which[101,]


