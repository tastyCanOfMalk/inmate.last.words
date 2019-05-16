library(ggplot2)
library(dplyr)
library(gridExtra)

df <- data.frame(
  "Race" = c("Hispanic","White","Black","White","Hispanic","Black","White",
             "White","Hispanic","White","White","Hispanic","White","White",
             "Black","Hispanic","Hispanic","Hispanic","Black","White"),
  "AWR" = c(47,38,46,61,44,43,48,58,38,33,58,43,35,43,36,33,35,27,46,67),
  "AgeExec" = c(28,22,34,36,22,30,37,45,21,24,40,19,27,30,22,20,20,22,33,36),
  "Served" = c(19,16,12,25,22,13,11,13,17,9,18,24,8,13,14,13,15,5,13,31))

g1 <- df %>% 
  ggplot(aes(x=as.factor(Race),y=AWR))+
  geom_boxplot()+
  geom_jitter(shape=16,position=position_jitter(.1),aes(color=Race))+
  coord_flip()+
  ggtitle("Race vs Age when received")+
  xlab("Race") + ylab("Age when received")+
  theme(legend.position = "none")
g2 <- df %>% 
  ggplot(aes(x=as.factor(Race),y=AgeExec))+
  geom_boxplot()+
  geom_jitter(shape=16,position=position_jitter(.1),aes(color=Race))+
  coord_flip()+
  ggtitle("Race vs Age when executed")+
  xlab("Race") + ylab("Age when executed")+
  theme(legend.position = "none")
g3 <- df %>% 
  ggplot(aes(x=as.factor(Race),y=Served))+
  geom_boxplot()+
  geom_jitter(shape=16,position=position_jitter(.1),aes(color=Race))+
  coord_flip()+
  ggtitle("Race vs time served")+
  xlab("Race") + ylab("Years served")+
  theme(legend.position = "none")

grid.arrange(g1,g2,g3,ncol=1)
