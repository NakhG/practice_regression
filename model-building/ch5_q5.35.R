#Chapter 5, Model Building
#Question 5.35, pg. 314
#Lead in fern moss

# Uses dataset LEADMOSS
#LEADMOSS <- load("~/LEADMOSS.Rdata")
head(LEADMOSS)

model <- lm(LEAD ~ ELEVATION*SLOPE, data= LEADMOSS)
plot(x=)

summary(model)

# y=2.384 + 0.0018(ELEVATION)+3.201(SLOPE) - 0.001(ELEVATION * SLOPE)

LEADMOSS$west.line <- 2.384 +0.0018*(LEADMOSS$ELEVATION)
LEADMOSS$east.line <- 2.384 + 0.0018*(LEADMOSS$ELEVATION)+3.201*(1) - 0.001*(LEADMOSS$ELEVATION * 1)

library(ggplot2)
ggplot(data=LEADMOSS, aes(x=ELEVATION, y=LEAD, color=(SLOPE==1))) +
  geom_point() +geom_line(LEADMOSS$west.line)

agg=aggregate(LEAD ~ ELEVATION+SLOPE,LEADMOSS, mean)
interaction.plot(agg$ELEVATION,agg$SLOPE, agg$LEAD,main="Interaction plot", xlab="Elevation", ylab="lead level", lwd=2)
