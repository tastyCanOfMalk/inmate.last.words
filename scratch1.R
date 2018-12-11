
bw.Served <- diff(range(x$Served)) / (2 * IQR(x$Served) / length(x$Served)^(1/3))

x %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=Served,fill=Race))+
  geom_histogram(bins=bw.Served)+
  facet_wrap(Race~.)

# shows an interesting large peak around 7.5 years
x %>% 
  filter(Race != "Other") %>% 
  ggplot(aes(x=Served,y=Race,fill=Race))+
  stat_binline(bins=bw.Served)+
  scale_x_continuous(limits=c(0,40),
                     breaks=c(seq(0,40,5)))


# harsher punishments/quicker death for more victims killed?
glimpse(x)
x %>% 
  mutate(VictimsTotal = FemaleVictim+MaleVictim) %>% 
  count(VictimsTotal)
# Graph victimstotal < 5 to keep clean
x %>% 
  mutate(VictimsTotal = as.factor(FemaleVictim+MaleVictim)) %>% 
  filter(as.numeric(VictimsTotal) < 5) %>% 
  ggplot(aes(x=Served,y=VictimsTotal))+
  geom_density_ridges()

x %>% 
  mutate(VictimsTotal = as.factor(FemaleVictim+MaleVictim)) %>% 
  filter(as.numeric(VictimsTotal) < 5) %>% 
  ggplot(aes(x=Served,y=Race))+
  geom_density_ridges()

glimpse(xx)

