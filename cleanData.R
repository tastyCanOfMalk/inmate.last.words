if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss
if(!require(VIM)) install.packages("VIM")
library(VIM) # aggr plot
if(!require(mice)) install.packages("mice")
library(mice) # impute missing data

# setwd("/home/e/R/inmate.last.words")
setwd("I:/Code-CAD/R/inmate.last.words")
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

x1 <- x %>% 
  mutate(AWR = AgeWhenReceived) %>% 
  mutate(AgeExec = Age) %>% 
  select(FirstName, 
         LastName, 
         Race, 
         EducationLevel,
         PreviousCrime, 
         NumberVictim, 
         FemaleVictim, 
         MaleVictim, 
         AWR,
         AgeExec, 
         Codefendants, 
         LastStatement)

aggr_plot <- aggr(x1, col=c('navyblue','red'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(data), 
                  cex.axis=.6,
                  gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))

# not really sure how MICE package works but it seems to fill in the data nicely
# mice library

# remove non numeric data to enable stripplot visualization
rem <- c(1,2,3,12)

tempData <- mice(x1[-rem],
                 maxit=20,
                 meth='pmm',
                 seed=1)

# summary(tempData)

# inspect distribution of imputed and original data using various plots
xyplot(tempData,
       AWR~
         PreviousCrime+
         EducationLevel+
         FemaleVictim+
         MaleVictim+
         NumberVictim+
         Codefendants,
       pch=18,
       cex=1)

densityplot(tempData)

stripplot(tempData,pch=20,cex=1)

# imputed values look good

# rerun imputation on complete dataset and compare to original
tempData <- mice(x1,m=5,maxit=50,meth='pmm')

# replace missing values with imputed values using first dataset
completedData <- complete(tempData,1)

# comparing the completed data to the original again shows imputed data makes sense
summary(completedData)[,-rem]
summary(x1)[,-rem]

