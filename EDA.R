if(!require(tibble)) install.packages("tibble")
library(tibble)
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(ggridges)) install.packages("ggridges")
library(ggridges)
if(!require(viridis)) install.packages("viridis")
library(viridis)
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)

setwd("C:/Users/e/Documents/R/inmate.last.words")
x <- read.csv("data/Texas Last Statement - CSV.csv", stringsAsFactors = F)

glimpse(x)
summary(x)

# EDA, focus on demographic
y <- x %>%
  select(Age,Race,AgeWhenReceived,EducationLevel,PreviousCrime) %>%
  mutate(Served = Age-AgeWhenReceived)
summary(y)

## Age of execution histogram
x %>% 
  ggplot(aes(x=Age))+
  geom_histogram()+
  scale_x_continuous(limits=c(min(y$Age),max(y$Age)),
                     breaks=c(seq(20,70,5)))+
  ggtitle("Age of execution")

## Segregate by race
summary(y$Race)

p1 <- x %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=Age,y=Race,fill=..density..))+
  geom_density_ridges_gradient(scale=1.5,rel_min_height=.01)+
  scale_x_continuous(limits=c(10,70),
                     breaks=c(seq(15,70,5)))+
  ggtitle("Age of execution by race")+
  xlab("Age when executed")+
  theme(legend.position="none")+
  scale_fill_viridis(option="E")
p1

## Why are whites executed at a later age?
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
  scale_fill_viridis(option="E")
p2

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
  facet_wrap(Race~.)

  
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
  
  
  
y.a <- setDT(y)[, .N, by = .(Age)][order(-N)]
plot(y.a)


ggplot(y,aes(x=Age,fill=Race))+geom_histogram()+facet_wrap(Race~.)

ggplot(y,aes(x=Served,fill=Race))+geom_histogram()+facet_wrap(Race~.)
ggplot(y,aes(x=Age,y=Race,fill=Race))+geom_density_ridges(aes(alpha=.5))
