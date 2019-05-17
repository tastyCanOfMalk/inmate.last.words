library(ggplot2)
library(dplyr)
library(gridExtra)

df <- data.frame(
  "Race" = c("Hispanic","White","Black","White","Hispanic","Black","White",
             "White","Hispanic","White","White","Hispanic","White","White",
             "Black","Hispanic","Hispanic","Hispanic","Black","White"),
  "AgeExec" = c(47,38,46,61,44,43,48,58,38,33,58,43,35,43,36,33,35,27,46,67),
  "AWR" = c(28,22,34,36,22,30,37,45,21,24,40,19,27,30,22,20,20,22,33,36),
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

# grid.arrange(g1,g2,g3,ncol=1)

# from reddit
df %>%
  gather(key = "Age.thingy", value = "Years", colnames(df)[2:4]) %>%
  mutate(Race = factor(Race)) -> df2

ggplot(data = df2, aes(x = Race, y = Years, fill = Race))+
  geom_boxplot()+
  geom_point()+
  coord_flip()+
  facet_wrap(~Age.thingy, nrow = 3, strip.position = "top")+
  theme_bw()
##

df <- xCom %>% 
  filter(Race != "Other") %>% 
  select(Race,AWR,AgeExec,Served)

df <- df %>% 
  gather(key = "Age.thingy", value = "Years", colnames(df)[2:4]) %>%
  mutate(Race = factor(Race))

ggplot(data = df, aes(x = Race, y = Years, fill = Race))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(shape=16,position=position_jitter(.15),alpha=.3)+
  coord_flip()+
  facet_wrap(~Age.thingy, nrow = 3, strip.position = "top")+
  theme_bw()

library(ggalt)

df.db <- xCom %>% 
  filter(Race != 'Other') %>% 
  group_by(Race) %>% 
  summarise(AWR=median(AWR),
            AgeExec=median(AgeExec),
            Served=median(Served))

df.db %>% 
  ggplot()+
  # geom_segment(aes(y=Race,yend=Race,x=0,xend=1),color="Red",size=.15)+
  geom_dumbbell(aes(y=Race, x=AWR, xend=AgeExec),
                size=5, 
                color="#e3e2e1",
                colour_x = "#5b8124", 
                colour_xend = "#bad744")+
  xlab("Age")+
  theme_bw()

df.db %>% 
  ggplot()+
  geom_crossbar(aes(ymin=AWR,
                    ymax=AgeExec,
                    x=as.factor(Race),
                    y=AWR),
                fatten=0)
