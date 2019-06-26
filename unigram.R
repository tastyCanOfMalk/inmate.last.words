#### List of top unigrams
y <- x$LastStatement
y <- tibble(y)

custom_stop_words <- 
  bind_rows(stop_words,
            tibble(word = c("ya'll", 
                            "y'all",
                            "warden"),
                   lexicon ="custom"))

# tidy the text
y.tidy <- y %>% 
  unnest_tokens(word,y) %>% 
  anti_join(custom_stop_words)

# sorted list of top words
y.tidy %>% 
  count(word, sort = TRUE)

#### Bar graph of top unigrams
# plot of above list of top used words
y.tidy %>% 
  count(word, sort = TRUE) %>% 
  filter(n>80) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  theme_bw()

x.unigrams <- x %>% 
  filter(Race != "Other") %>% 
  select(Race, LastStatement) %>% 
  unnest_tokens(unigram,LastStatement,token="ngrams",n=1)

# x.unigrams %>% 
#   count(unigram,sort=T)

x.unigrams.separated <- x.unigrams %>% 
  separate(unigram, c("word1"),sep=" ")

x.unigrams.filtered <- x.unigrams.separated %>% 
  filter(!word1 %in% stop_words$word)

x.unigram.counts <- x.unigrams.filtered %>% 
  count(word1,sort=T)

# recombine unigrams without stop words
x.unigrams.united <- x.unigrams.filtered %>% 
  unite(unigram,word1,sep=" ") 

# wordcloud of most common unigrams ALL
x.unigrams.united %>% 
  count(unigram) %>% 
  with(wordcloud(unigram,n,
                 max.words=80,
                 min.freq=5,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5)))


x.unigrams <- x %>%
  filter(Race != "Other") %>% 
  select(Race, LastStatement) %>% 
  unnest_tokens(unigram,LastStatement,token="ngrams",n=1) %>% 
  separate(unigram, c("word1"),sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  unite(unigram,word1,sep=" ") 





# wordcloud of most common unigrams WHITE
x.unigrams.united %>%
  filter(Race == "White") %>% 
  count(unigram) %>% 
  with(wordcloud(unigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5))) 

# wordcloud of most common unigrams BLACK
x.unigrams.united %>%
  filter(Race == "Black") %>% 
  count(unigram) %>% 
  with(wordcloud(unigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5))) 

# wordcloud of most common unigrams HISPANIC
x.unigrams.united %>%
  filter(Race == "Hispanic") %>% 
  count(unigram) %>% 
  with(wordcloud(unigram,n,
                 max.words=80,
                 min.freq=3,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(4,.5))) 

