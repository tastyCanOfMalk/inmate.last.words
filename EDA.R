# if(!require(tibble)) install.packages("tibble")
# library(tibble)
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(naniar)) install.packages("naniar")
library(naniar) # gg_miss
if(!require(ggridges)) install.packages("ggridges")
library(ggridges)
if(!require(viridis)) install.packages("viridis")
library(viridis)
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
if(!require(data.table)) install.packages("data.table")
library(data.table)

setwd("/home/e/R/inmate.last.words")
# setwd("C:/Users/e/Documents/R/inmate.last.words")
x <- read_csv("data/Texas Last Statement - CSV.csv")

# Find unique levels
x.levels <- cbind(colnames(x),
                  (as.data.frame(sapply(x,function(x) length(unique(x))))))
colnames(x.levels) <- c("var","levels")
row.names(x.levels) <- NULL
x.levels[order(-x.levels[,2]),]

# get proportion of NA/missing data
gg_miss_var(x, show_pct = T)
gg_miss_which(x)

# glimpse(x)
# summary(x)

# Fill in some missing values
## I want to add a $Served column but this requires complete data on AgeWhenReceived
x %>% 
  select(Race, AgeWhenReceived) %>% 
  na.omit() %>% 
  ggplot(aes(x=AgeWhenReceived,fill=Race)) +
  geom_density(aes(alpha=.5))

### Calculate mean AWR for each Race and replace NA's with that
x %>% 
  filter(Race=="Hispanic") %>% 
  na.omit %>% 
  summarise(meanRecd = round(mean(AgeWhenReceived),0))
x <- x %>% 
  mutate(AgeWhenReceived = replace(AgeWhenReceived,
                                   is.na(AgeWhenReceived)&
                                     Race=="Hispanic",27))

x %>% 
  filter(Race=="Black") %>% 
  na.omit %>% 
  summarise(meanRecd = round(mean(AgeWhenReceived),0))
x <- x %>% 
  mutate(AgeWhenReceived = replace(AgeWhenReceived,
                                   is.na(AgeWhenReceived)&
                                     Race=="Black",26))

x %>% 
  filter(Race=="White") %>% 
  na.omit %>% 
  summarise(meanRecd = round(mean(AgeWhenReceived),0))
x <- x %>% 
  mutate(AgeWhenReceived = replace(AgeWhenReceived,
                                   is.na(AgeWhenReceived)&
                                     Race=="White",31))
## Now we can add a Served column
x <- x %>% 
  mutate(Served = Age-AgeWhenReceived)

which(is.na(x$Served)) # verify no NA values

nums <- which(is.na(x$NumberVictim))
nums1 <- x[nums,]

## White vs Black/Hispanic have somewhat differing values, fill in based on mean


# EDA, focus on demographic
x <- x %>% 
  mutate(Served=Age-AgeWhenReceived)

# y <- x %>%
#   select(Age,Race,AgeWhenReceived,EducationLevel,PreviousCrime) %>%
#   mutate(Served = Age-AgeWhenReceived) %>%
#   mutate(Race=as.factor(Race))
# summary(y)

## Age of execution histogram
# bins calculated using Freedman–Diaconis rule
bw.Age <- diff(range(x$Age)) / (2 * IQR(x$Age) / length(x$Age)^(1/3))
x %>% 
  ggplot(aes(x=Age))+
  geom_histogram(bins=bw.Age)+
  scale_x_continuous(limits=c(min(x$Age),max(x$Age)),
                     breaks=c(seq(20,70,5)))+
  ggtitle("Age of execution")

## Segregate by race
summary(as.factor(x$Race))

p1 <- x %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=Age,y=Race,fill=..density..))+
  geom_density_ridges_gradient(scale=1.5,rel_min_height=.01)+
  scale_x_continuous(limits=c(10,70),
                     breaks=c(seq(15,70,5)))+
  ggtitle("Age of execution by race")+
  xlab("Age when executed")+
  theme(legend.position="none")+
  scale_fill_viridis(option="D")
p1

## Are whites executed at a later age?
## Let's check AgeWhenReceived distribution
p2 <- x %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=AgeWhenReceived,y=Race,fill=..density..))+
  geom_density_ridges_gradient(scale=1.5,rel_min_height=.01)+
  scale_x_continuous(limits=c(10,70),
                     breaks=c(seq(15,70,5)))+
  ggtitle("Age when received by race")+
  xlab("Age when received")+
  theme(legend.position="none")+
  scale_fill_viridis(option="D")
p2

# stat binline version
p2.1 <- x %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=AgeWhenReceived,y=Race,fill=Race))+
  stat_binline(scale=.9, bins=bw.Age)+
  scale_x_continuous(limits=c(10,70),
                     breaks=c(seq(15,70,5)))+
  ggtitle("Age when received by race")+
  xlab("Age when received")+
  theme(legend.position="none")
p2.1

grid.arrange(p2,p1,nrow=2)

## Seems like whites are received at a later age then hispanic or black,
## let's see if years served are basically the same
## And years served by race
p3 <- x %>% 
  filter(Race != "Other") %>% 
  mutate(Served = Age-AgeWhenReceived) %>% 
  ggplot(aes(x=Served,y=Race,fill=..density..))+
  geom_density_ridges_gradient(scale=1.5,rel_min_height=.01)+
  scale_x_continuous(limits=c(0,40),
                     breaks=c(seq(0,40,5)))+
  ggtitle("Years served by race")+
  xlab("Years served")+
  theme(legend.position="none")+
  scale_fill_viridis(option="E")
p3

## absolute frequency

x %>% 
  filter(Race != "Other") %>% 
  mutate(Served = Age-AgeWhenReceived) %>% 
  ggplot(aes(x=Served,fill=Race))+
  geom_histogram()+
  facet_wrap(Race~.)
  
## relative frequency
x %>% 
  filter(Race != "Other") %>% 
  mutate(Served = Age-AgeWhenReceived) %>% 
  ggplot(aes(x=Served,fill=Race))+
  geom_density(alpha=.8)
  # facet_wrap(Race~.)

  
x %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(fill=Race))+
  geom_density_ridges(scale=1,rel_min_height=.01,alpha=.5,aes(x=Age,y=Race))+
  geom_density_ridges(scale=1,rel_min_height=.01,alpha=.5,aes(x=AgeWhenReceived, y=Race))+
  scale_x_continuous(limits=c(10,70),
                     breaks=c(seq(15,70,5)))+
  ggtitle("Age of execution by race")+
  xlab("Age when executed")+
  theme(legend.position="none")
  
  
  
y.a <- setDT(x)[, .N, by = .(Age)][order(-N)]
plot(y.a)


ggplot(x,aes(x=Age,fill=Race))+geom_histogram()+facet_wrap(Race~.)

ggplot(x,aes(x=Served,fill=Race))+geom_histogram()+facet_wrap(Race~.)
ggplot(x,aes(x=Age,y=Race,fill=Race))+geom_density_ridges(aes(alpha=.5))
