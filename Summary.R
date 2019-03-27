if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss
if(!require(mice)) install.packages("mice")
library(mice) # gg_miss
if(!require(ggridges)) install.packages("ggridges")
library(ggridges)
if(!require(viridis)) install.packages("viridis")
library(viridis)
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
if(!require(GGally)) install.packages("GGally")
library(GGally)

# clear history
rm(list = ls())

# setwd("I:/Code-CAD/R/inmate.last.words")
setwd("D:/Code/R/inmate.last.words")

# import
x <- read_csv("data/Texas Last Statement - CSV.csv")

glimpse(x)
summary(x)

# Find unique levels
x.levels <- cbind(colnames(x),
                  (as.data.frame(sapply(x,function(x) length(unique(x))))))
colnames(x.levels) <- c("var","levels")
row.names(x.levels) <- NULL
x.levels[order(-x.levels[,2]),]

# % missingness using naniar package
gg_miss_var(x, show_pct = T)

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

# remove columns with characters for stripplot() visualization
rem <- c(1,2,3,12)

# imputation process using pmm, 20 iterations
tempData <- mice(x1[-rem],
                 maxit=20,
                 meth='pmm',
                 seed=1)

# compare distribution of actual values with imputed values; red=imputed
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

# can also compare using density plot; red=imputed
densityplot(tempData)

# and stripplot
stripplot(tempData,pch=20,cex=1)

# rerun imputation on complete dataset and compare to original
tempData <- mice(x1,m=5,maxit=50,meth='pmm')

# replace missing values with imputed values using first dataset
xCom <- complete(tempData,1)

# verfy no missing data
gg_miss_var(xCom, show_pct = T)

# add time served measure
xCom <- xCom %>% 
  mutate(Served = AgeExec-AWR)

# have an odd -1 years served
summary(xCom$Served)

# find datapoint
which.min(xCom$Served)
xCom[266,]

# google search reveals execution at 47
# change AgeExec and Served for 266
xCom[266,]$AgeExec <- 47
xCom[266,]$Served <- 10

################ EDA

# check races, remove other when plotting
summary(as.factor(x$Race))

xCom %>% 
  filter(Race != "Other") %>% 
  group_by(Race) %>% 
  summarise(median(Served))

g1 <- xCom %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=Served,fill=Race))+
  geom_density(alpha=.8)+
  ggtitle("Time served by race (relative)")

g2 <- xCom %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=Served,fill=Race))+
  geom_histogram()+
  facet_wrap(Race~.)+
  ggtitle("Time served by race (absolute)")

grid.arrange(g1,g2,nrow=1)

# but what about age received and executed?
# race vs median AWR
xCom %>% 
  filter(Race != "Other") %>% 
  group_by(Race) %>% 
  summarise(median(AWR))

# Age received vs race
p1 <- xCom %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=AWR,y=Race,fill=..density..))+
  geom_density_ridges_gradient(scale=1.5,rel_min_height=.01)+
  scale_x_continuous(limits=c(10,70),
                     breaks=c(seq(15,70,5)))+
  ggtitle("Age when received by race")+
  xlab("Age when received")+
  theme(legend.position="none")+
  scale_fill_viridis(option="D")
p1

# Age executed vs race
p2 <- xCom %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=AgeExec,y=Race,fill=..density..))+
  geom_density_ridges_gradient(scale=1.5,rel_min_height=.01)+
  scale_x_continuous(limits=c(10,70),
                     breaks=c(seq(15,70,5)))+
  ggtitle("Age of execution by race")+
  xlab("Age when executed")+
  theme(legend.position="none")+
  scale_fill_viridis(option="D")
p2

grid.arrange(p1,p2,nrow=1)

# number of victims?
hist(xCom$NumberVictim)
hist(xCom$EducationLevel)
hist(xCom$PreviousCrime)
hist(xCom$FemaleVictim)
hist(xCom$MaleVictim)

# does time served change as number of victims change?
xCom %>% 
  filter(Served > 0 & Served < 6) %>% 
  ggplot(aes(x=Served))+
  geom_histogram(stat="count")+
  facet_wrap(NumberVictim~.,nrow=3)+
  ggtitle("Years served faceted by number of victims")

# education level vs number of victims
e1 <- xCom %>% 
  filter(NumberVictim > 0 & NumberVictim < 5) %>%
  ggplot(aes(x=EducationLevel))+
  geom_histogram(stat="count")+
  facet_wrap(NumberVictim~.,nrow=2)+
  ggtitle("Education level faceted by number of victims")

e2 <- xCom %>% 
  ggplot(aes(x=EducationLevel))+
  geom_histogram(stat="count")+
  scale_x_continuous(breaks=seq(1,16,1))+
  ggtitle("Histogram of education level")

grid.arrange(e1,e2,nrow=1)



xCom %>% 
  filter(Race != "Other") %>% 
  filter(NumberVictim > 0 & NumberVictim < 5) %>%
  ggplot(aes(x=AgeExec,y=Race,fill=..density..))+
  geom_density_ridges_gradient(scale=1.5,rel_min_height=.01)+
  scale_x_continuous(limits=c(10,70),
                     breaks=c(seq(15,70,5)))+
  facet_wrap(NumberVictim~.)

xCom %>% 
  filter(Race!="Other") %>% 
  filter(NumberVictim > 0 & NumberVictim < 5) %>%
  ggplot(aes(x=as.factor(Race),fill=Race))+
  geom_histogram(stat="count")+
  facet_wrap(NumberVictim~.)

xCom %>% 
  filter(NumberVictim > 0 & NumberVictim < 5) %>%
  ggplot(aes(x=Served))+
  geom_histogram(stat="count")+
  facet_wrap(NumberVictim~.)

xCom %>% 
  ggplot(aes(x=Served))+
  geom_histogram(stat="count")+
  facet_wrap(FemaleVictim~.)
xCom %>% 
  ggplot(aes(x=AWR))+
  geom_histogram(stat="count")+
  facet_wrap(NumberVictim~.)
xCom %>% 
  ggplot(aes(x=AWR))+
  geom_histogram(stat="count")+
  facet_wrap(EducationLevel~.)

xCom %>% 
  filter(EducationLevel>8 &
           EducationLevel<15) %>%  
  ggplot(aes(x=AWR))+
  geom_histogram(stat="count")+
  facet_wrap(EducationLevel~.)
