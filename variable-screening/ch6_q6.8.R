#Ch. 6, Variable Screening Methods
#Question 6.8, pg. 341
#Collusive bidding in road construction


#dataset flag2
#FLAG2 <- load("~/DISTRICT.Rdata")
head(FLAG2)
#clean up remove whitespace in entries
paste0(FLAG2$DISTRICT)
#remove na's
FLAG2.scrubbed <- na.omit(FLAG2)


#create dummy variables
FLAG2$MYDISTRICT_1 <- ifelse(FLAG2$DISTRICT =="1", 1,0)
FLAG2$MYDISTRICT_2 <- ifelse(FLAG2$DISTRICT =="2", 1,0)
FLAG2$MYDISTRICT_3 <- ifelse(FLAG2$DISTRICT =="3", 1,0)
FLAG2$MYDISTRICT_4 <- ifelse(FLAG2$DISTRICT =="4", 1,0)

#do stepwise

model.init1 <- lm(LOWBID ~ DOTEST + LBERATIO + STATUS + NUMBIDS + DAYSEST + RDLNGTH + PCTASPH + PCTBASE + PCTEXCAV + PCTMOBIL + PCTSTRUC + PCTTRAFF + SUBCONT + MYDISTRICT_1 + MYDISTRICT_2 + MYDISTRICT_3 + MYDISTRICT_4, data=FLAG2.scrubbed)
model1 = step(model.init1, direction="both")
summary(model1)
model1
