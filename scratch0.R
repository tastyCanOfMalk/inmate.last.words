if(!require(tibble)) install.packages("tibble")
library(tibble)
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(ggridges)) install.packages("ggridges")
library(ggridges)

setwd("C:/Users/e/Documents/R/inmate.last.words")
x <- read.csv("data/Texas Last Statement - CSV.csv")

glimpse(x)
summary(x)

# EDA, focus on demographic

y <- x %>% 
  select(Age,Race,AgeWhenReceived,EducationLevel) %>% 
  mutate(Served = Age-AgeWhenReceived)

summary(y)

ggplot(y,aes(x=Age,fill=Race))+geom_histogram()+facet_wrap(Race~.)
ggplot(y,aes(x=Served,fill=Race))+geom_histogram()+facet_wrap(Race~.)
ggplot(y,aes(x=Age,y=Race,fill=Race))+geom_density_ridges(aes(alpha=.5))
