# corrplot to start with?
if(!require(GGally)) install.packages("GGally")
library(GGally) # impute missing data


glimpse(x)
x.pairs <- x %>% 
  select(Age,Race,AgeWhenReceived,EducationLevel,NativeCounty,
         PreviousCrime,Codefendants,NumberVictim)
ggpairs(x.pairs)
# Fill in missing values

## NumberVictim
### Use median, only 18 NA's
x <- x %>% 
  mutate(NumberVictim = replace(NumberVictim,
                                is.na(NumberVictim),
                                1)) %>% 
  mutate(Codefendants = replace(Codefendants,
                                is.na(Codefendants),
                                1)) %>% 
  mutate(EducationLevel = replace(EducationLevel,
                                  is.na(EducationLevel),
                                  10)) %>% 
  mutate(PreviousCrime = replace(PreviousCrime,
                                  is.na(PreviousCrime),
                                  1))
  
x %>% 
  count(PreviousCrime)

# https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
install.packages("mice")
library(mice)
md.pattern(x.pairs)
install.packages("VIM")
library(VIM)
aggr_plot <- aggr(x.pairs, col=c('navyblue','red'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(data), 
                  cex.axis=.7, 
                  gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
marginplot(x.pairs[c(1,3)])
marginplot(x.pairs[c(1,6)])
glimpse(x.pairs)




summary(x$EducationLevel)
summary(x$PreviousCrime)
summary(x$NumberVictim)
summary(x$Codefendants)

lm1 <- lm(Codefendants~Race+AgeWhenReceived,data=x)
summary(lm1)

x.black <- subset(x,Race=="Black")
x.white <- subset(x,Race=="White")
x.hispa <- subset(x,Race=="Hispanic")
summary(x.black$Codefendants)
summary(x.white$Codefendants)
summary(x.hispa$Codefendants)
# looks like we could either assign codefendent values based on race
# or use the linear model to be more specific, however, getting specific might
# be a waste of time as 1.4 defendants would be the same as 0.8
# median or mean?
# use tree?


# race vs median AWR
x.med.awr <- xCom %>% 
  filter(Race != "Other") %>% 
  group_by(Race) %>% 
  summarise(median(AWR))

# race vs median AgeExecuted
x.med.awe <- xCom %>% 
  filter(Race != "Other") %>% 
  group_by(Race) %>% 
  summarise(median(AWR))
  summarise(median(AgeExec))

rec.exec <- xCom %>% 
  filter(Race != "Other") %>% 
  group_by(Race) %>% 
  summarise(AWR=median(AWR),AgeExec=median(AgeExec)) %>% 
  mutate(Served = AgeExec-AWR)

melt(rec.exec) %>% 
  ggplot(aes(x=Race,y=value))+
  geom_boxplot()+
  coord_flip()


xCom %>% 
  filter(Race != "Other") %>% 
  group_by(Race) %>%
  select(AgeExec,AWR) %>% 
  mutate(Served = AgeExec-AWR) %>% 
  ggplot(aes(x=as.factor(Race),y=AWR))+
  geom_boxplot()+
  geom_jitter(shape=16,position=position_jitter(.1),alpha=.2,aes(color=Race))+
  coord_flip()

tt <- xCom %>% 
  filter(Race != "Other") %>% 
  group_by(Race) %>%
  select(AgeExec,AWR) %>% 
  mutate(Served = AgeExec-AWR)

tt[1:20,4]


