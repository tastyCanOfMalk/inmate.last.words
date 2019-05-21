

x %>% 
  filter(str_detect(LastStatement,"crime")) %>% 
  select(LastStatement)

x %>% 
  filter(str_detect(LastStatement,"wrongfully")) %>% 
  select(LastStatement) %>% 
  unnest_tokens(word,LastStatement) %>% 
  anti_join(custom_stop_words) %>% 
  count(word,sort=T) %>% 
  ungroup() %>% 
  with(wordcloud(word,n,
                 max.words=80,
                 min.freq=7,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5)))
