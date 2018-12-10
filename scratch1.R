# shows an interesting large peak around 7.5 years
x %>% 
  filter(Race != "Other") %>% 
  mutate(Served = Age-AgeWhenReceived) %>% 
  ggplot(aes(x=Served,y=Race,fill=Race))+
  stat_binline(bins=10)+
  scale_x_continuous(limits=c(0,40),
                     breaks=c(seq(0,40,5)))


# harsher punishments/quicker death for more victims killed?
glimpse(x)
x %>% 
  mutate(Served = Age-AgeWhenReceived) %>% 
  mutate(VictimsTotal = FemaleVictim+MaleVictim) %>% 
  count(VictimsTotal)
# Graph victimstotal < 5 to keep clean
x %>% 
  mutate(Served = Age-AgeWhenReceived) %>% 
  mutate(VictimsTotal = as.factor(FemaleVictim+MaleVictim)) %>% 
  filter(as.numeric(VictimsTotal) < 5) %>% 
  ggplot(aes(x=Served,y=VictimsTotal))+
  geom_density_ridges()

x %>% 
  mutate(Served = Age-AgeWhenReceived) %>% 
  mutate(VictimsTotal = as.factor(FemaleVictim+MaleVictim)) %>% 
  filter(as.numeric(VictimsTotal) < 5) %>% 
  ggplot(aes(x=Served,y=Race))+
  geom_density_ridges()

glimpse(xx)

x %>% 
  filter(Race != "Other") %>% 
  mutate(Served = Age-AgeWhenReceived) %>% 
  ggplot(aes(x=Served,fill=Race))+
  geom_histogram()+
  facet_wrap(Race~.)
