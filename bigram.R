# #### List of top bigrams
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
# #### Bar graph of top bigrams
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
# x.bigrams <- x %>% 
#   filter(Race != "Other") %>% 
#   select(Race, LastStatement) %>% 
#   unnest_tokens(bigram,LastStatement,token="ngrams",n=2)
# 
# # x.bigrams %>% 
# #   count(bigram,sort=T)
# 
# x.bigrams.separated <- x.bigrams %>% 
#   separate(bigram, c("word1"),sep=" ")
# 
# x.bigrams.filtered <- x.bigrams.separated %>% 
#   filter(!word1 %in% stop_words$word)
# 
# x.bigram.counts <- x.bigrams.filtered %>% 
#   count(word1,sort=T)
# 
# # recombine bigrams without stop words
# x.bigrams.united <- x.bigrams.filtered %>% 
#   unite(bigram,word1,sep=" ") 
# 
# # wordcloud of most common bigrams ALL
# x.bigrams.united %>% 
#   count(bigram) %>% 
#   with(wordcloud(bigram,n,
#                  max.words=80,
#                  min.freq=5,
#                  colors=pal,
#                  random.order = F,
#                  random.color = F,
#                  scale=c(3,.5)))


x.bigrams <- x %>%
  filter(Race != "Other") %>% 
  select(Race, LastStatement) %>% 
  unnest_tokens(bigram,LastStatement,token="ngrams",n=2) %>% 
  separate(bigram, c("word1","word2"),sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigram,word1,word2,sep=" ") 


# wordcloud of most common bigrams ALL
x.bigrams%>%
  count(bigram) %>% 
  with(wordcloud(bigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5))) 

# wordcloud of most common bigrams WHITE
x.bigrams%>%
  filter(Race == "White") %>% 
  count(bigram) %>% 
  with(wordcloud(bigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5))) 

# wordcloud of most common bigrams BLACK
x.bigrams %>%
  filter(Race == "Black") %>% 
  count(bigram) %>% 
  with(wordcloud(bigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5))) 

# wordcloud of most common bigrams HISPANIC
x.bigrams %>%
  filter(Race == "Hispanic") %>% 
  count(bigram) %>% 
  with(wordcloud(bigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(4,.5))) 


x.bigram.tf.idf <- x.bigrams %>% 
  count(Race,bigram) %>% 
  bind_tf_idf(bigram,Race,n) %>% 
  arrange(desc(tf_idf))

x.bigram.tf.idf %>% 
  arrange(desc(tf_idf)) %>%
  mutate(bigram=factor(bigram,levels=rev(unique(bigram)))) %>%
  group_by(Race) %>% 
  top_n(5) %>% 
  ungroup() %>%
  ggplot(aes(bigram, tf_idf,fill=Race))+
  geom_col(show.legend=F)+
  labs(x=NULL,y="tf-df")+
  facet_wrap(~Race,scale="free")+
  coord_flip()
