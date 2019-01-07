if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss
if(!require(VIM)) install.packages("VIM")
library(VIM) # aggr plot
if(!require(mice)) install.packages("mice")
library(mice) # impute missing data

# setwd("/home/e/R/inmate.last.words")
setwd("Y:/Code-CAD/R/inmate.last.words")
x <- read_csv("data/Texas Last Statement - CSV.csv")

# get proportion of NA/missing data
gg_miss_var(x, show_pct = T)
gg_miss_which(x)

# proportion missing + pattern of missing
aggr_plot <- aggr(x, col=c('navyblue','red'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(data), 
                  cex.axis=.5,
                  gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))

# There's a fair amount of missing data
# Likely not the most useful filling in all of it
# Let's choose a few that might be interesting to explore

x.1 <- x %>% 
  mutate(AWR = AgeWhenReceived) %>% 
  mutate(AgeExec = Age) %>% 
  select(FirstName, LastName, LastStatement, Race, AgeExec, AWR,
         NumberVictim, FemaleVictim, MaleVictim, Codefendants, PreviousCrime, EducationLevel)

aggr_plot <- aggr(x.1, col=c('navyblue','red'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(data), 
                  cex.axis=.6,
                  gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))

# not really sure how MICE package works but it seems to fill in the data nicely
# mice library
rem <- c(1,2,3,4,5) # remove complete data for visualizations
tempData <- mice(x.1[-rem],m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
completedData <- complete(tempData,1)
xyplot(tempData,EducationLevel~
         PreviousCrime+Codefendants+FemaleVictim+MaleVictim+AWR+NumberVictim,pch=18,cex=1)
densityplot(tempData)
stripplot(tempData,pch=20,cex=1)

# red values are imputed data, seems to look okay, lets repeat the imputation with the missing character data
tempData <- mice(x.1,m=5,maxit=50,meth='pmm')
x.com <- as_data_frame(complete(tempData,1))

# comparing the completed data to the original again shows imputed data makes sense
summary(x.com)
summary(x.1)

