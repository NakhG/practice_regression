#HW4 q5.8 
#Cooling method for gas turbines.
#Small multiples / correlation tables

#import dataset
GASTURBINE <- load("C:/Users/George/Downloads/rdata/GASTURBINE.Rdata")
head(GASTURBINE)

#Graph scatterplots of heat rate against all other variables

#use package that will format for easire understanding of grid of scatterplots
# Scatterplot Matrices from the glus Package 
library(gclus)
library("dplyr", lib.loc="~/R/win-library/3.2")

gasturbine.cors <- abs(cor(GASTURBINE)) # get correlations
gasturbine.col <- dmat.color(gasturbine.cors) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
gasturbine.order <- order.single(gasturbine.cors) 
cpairs(GASTURBINE, gasturbine.order, panel.colors=gasturbine.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

#says that some objects aren't numeric
lapply(GASTURBINE, class)

gasturbine.refine <- select(GASTURBINE, -ENGINE)
head(gasturbine.refine)

gasturbine.cors <- abs(cor(gasturbine.refine)) # get correlations
gasturbine.col <- dmat.color(gasturbine.cors) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
gasturbine.order <- order.single(gasturbine.cors) 
cpairs(gasturbine.refine, gasturbine.order, panel.colors=gasturbine.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )


