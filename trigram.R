# #### List of top trigrams
# y <- x$LastStatement
# y <- tibble(y)
# 
# custom_stop_words <- 
#   bind_rows(stop_words,
#             tibble(word = c("ya'll", 
#                             "y'all",
#                             "warden"),
#                    lexicon ="custom"))
# 
# # tidy the text
# y.tidy <- y %>% 
#   unnest_tokens(word,y) %>% 
#   anti_join(custom_stop_words)
# 
# # sorted list of top words
# y.tidy %>% 
#   count(word, sort = TRUE)
# 
# #### Bar graph of top trigrams
# # plot of above list of top used words
# y.tidy %>% 
#   count(word, sort = TRUE) %>% 
#   filter(n>80) %>% 
#   mutate(word=reorder(word,n)) %>% 
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip()+
#   theme_bw()
# 
# x.trigrams <- x %>% 
#   filter(Race != "Other") %>% 
#   select(Race, LastStatement) %>% 
#   unnest_tokens(trigram,LastStatement,token="ngrams",n=2)
# 
# # x.trigrams %>% 
# #   count(trigram,sort=T)
# 
# x.trigrams.separated <- x.trigrams %>% 
#   separate(trigram, c("word1"),sep=" ")
# 
# x.trigrams.filtered <- x.trigrams.separated %>% 
#   filter(!word1 %in% stop_words$word)
# 
# x.trigram.counts <- x.trigrams.filtered %>% 
#   count(word1,sort=T)
# 
# # recombine trigrams without stop words
# x.trigrams.united <- x.trigrams.filtered %>% 
#   unite(trigram,word1,sep=" ") 
# 
# # wordcloud of most common trigrams ALL
# x.trigrams.united %>% 
#   count(trigram) %>% 
#   with(wordcloud(trigram,n,
#                  max.words=80,
#                  min.freq=5,
#                  colors=pal,
#                  random.order = F,
#                  random.color = F,
#                  scale=c(3,.5)))


x.trigrams <- x %>%
  filter(Race != "Other") %>% 
  select(Race, LastStatement) %>% 
  unnest_tokens(trigram,LastStatement,token="ngrams",n=3) %>% 
  separate(trigram, c("word1","word2","word3"),sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  unite(trigram,word1,word2,word3,sep=" ") 


# wordcloud of most common trigrams ALL
x.trigrams%>%
  count(trigram) %>% 
  with(wordcloud(trigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5))) 

# wordcloud of most common trigrams WHITE
x.trigrams%>%
  filter(Race == "White") %>% 
  count(trigram) %>% 
  with(wordcloud(trigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5))) 

# wordcloud of most common trigrams BLACK
x.trigrams %>%
  filter(Race == "Black") %>% 
  count(trigram) %>% 
  with(wordcloud(trigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5))) 

# wordcloud of most common trigrams HISPANIC
x.trigrams %>%
  filter(Race == "Hispanic") %>% 
  count(trigram) %>% 
  with(wordcloud(trigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(4,.5))) 


x.trigram.tf.idf <- x.trigrams %>% 
  count(Race,trigram) %>% 
  bind_tf_idf(trigram,Race,n) %>% 
  arrange(desc(tf_idf))

x.trigram.tf.idf %>% 
  arrange(desc(tf_idf)) %>%
  mutate(trigram=factor(trigram,levels=rev(unique(trigram)))) %>%
  group_by(Race) %>% 
  top_n(5) %>% 
  ungroup() %>%
  ggplot(aes(trigram, tf_idf,fill=Race))+
  geom_col(show.legend=F)+
  labs(x=NULL,y="tf-df")+
  facet_wrap(~Race,scale="free")+
  coord_flip()
