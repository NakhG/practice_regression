#hw 6
#Ch. 6, Variable Screening Methods
#question 6.9, pg. 343
#Collusive bidding in road construction

#Uses FLAG2 dataset
#FLAG2 <- load("~/DISTRICT.Rdata")

#Apply all-possible-regressions selection model to FLAG2 data
#are the variables in the 'best subset' model the same as those selected by stepwise regression?

#First things first, lets get an all-possible-regressions model

library(leaps)
yvar = c("LOWBID")
xvars = c("DOTEST" , "LBERATIO" , "STATUS" , "NUMBIDS" , "DAYSEST" , "RDLNGTH" , "PCTASPH" , "PCTBASE" , "PCTEXCAV" , "PCTMOBIL" , "PCTSTRUC" , "PCTTRAFF" , "SUBCONT" , "MYDISTRICT_1" , "MYDISTRICT_2" , "MYDISTRICT_3" , "MYDISTRICT_4")
model.allpossible=leaps(x=FLAG2[,xvars], y=FLAG2[,yvar], names=xvars, method="Cp")
model.allpossible$which

help("leaps")
#problem w/ the dataset: there are NAs
FLAG2.scrubbed <- na.omit(FLAG2)
yvar = c("LOWBID")
xvars = c("DOTEST" , "LBERATIO" , "STATUS" , "NUMBIDS" , "DAYSEST" , "RDLNGTH" , "PCTASPH" , "PCTBASE" , "PCTEXCAV" , "PCTMOBIL" , "PCTSTRUC" , "PCTTRAFF" , "SUBCONT" , "MYDISTRICT_1" , "MYDISTRICT_2" , "MYDISTRICT_3" , "MYDISTRICT_4")
model.allpossible=leaps(x=FLAG2.scrubbed[,xvars], y=FLAG2.scrubbed[,yvar], names=xvars, nbest= 3, method="Cp")
model.allpossible$which

model.allpossible$Cp
min(model.allpossible$Cp) #we see here it's the 13th row

model.allpossibe(model.allpossible$Cp == min(model.allpossible$Cp))

#results:
##     DOTEST LBERATIO STATUS NUMBIDS DAYSEST RDLNGTH PCTASPH PCTBASE PCTEXCAV PCTMOBIL PCTSTRUC PCTTRAFF SUBCONT MYDISTRICT_1 MYDISTRICT_2 MYDISTRICT_3 MYDISTRICT_4
##5    TRUE     TRUE  FALSE   FALSE   FALSE    TRUE    TRUE   FALSE    FALSE    FALSE    FALSE     TRUE   FALSE        FALSE        FALSE        FALSE        FALSE

#DOTEST, LBERATIO, RDLNGTH, PCTASPH, PCTTRAFF
