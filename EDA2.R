# after cleanData.R
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
# if(!require(GGally)) install.packages("GGally")
# library(GGally) # ggpairs

# x.com <- as_data_frame(x.complete)

summary(x.com)
class(x.com)
glimpse(x.com)

# add couple more measures
x.com <- x.com %>% 
  mutate(Served = AgeExec - AWR) %>% 
  mutate(VictimsTotal = MaleVictim + FemaleVictim) 

# unique variables
x.levels <- x.com %>% 
  summarise_all(funs(n_distinct))
x.levels <- t(x.levels[order(-x.levels)])
colnames(x.levels) <- c("unique")
data.frame(x.levels)



x.com %>% 
  









colnames(x.levels) <- c("var","levels")
row.names(x.levels) <- NULL
x.levels[order(-x.levels[,2]),]

distinct(x.com$FirstName)


x.com <- x.com %>% 
  mutate(Served = AgeExec - AWR) %>% 
  mutate(VictimsTotal = MaleVictim + FemaleVictim) 
  # mutate(Race = as_factor(x.com$Race))









# Find unique levels
x.levels <- cbind(colnames(x.complete),
                  (as.data.frame(sapply(x.complete,function(x) length(unique(x.complete)))))
)
colnames(x.levels) <- c("var","levels")
row.names(x.levels) <- NULL
x.levels[order(-x.levels[,2]),]

x.com %>%
  ggplot(aes(x=VictimsTotal))+
  geom_density(aes(y=Served))