if(!require(tidytext)) install.packages("tidytext")
library(tidytext)
if(!require(tibble)) install.packages("tibble")
library(tibble)
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(wordcloud)) install.packages("wordcloud")
library(wordcloud)
if(!require(reshape2)) install.packages("reshape2")
library(reshape2)
if(!require(ggraph)) install.packages("ggraph")
library(ggraph)
# may need to install.packages("geforce")
# linux may require sudo apt-get install libudunits2-dev

# setwd("C:/Users/e/Documents/R/inmate.last.words")
setwd("/home/e/R/inmate.last.words")

# read data, prepare text for tidying
x <- read_csv("data/Texas Last Statement - CSV.csv") %>% 
  filter(Race != "Other")
y <- x$LastStatement
y <- data_frame(y)

# Modify stop_words slightly
custom_stop_words <- 
  bind_rows(stop_words,
            data_frame(word = c("ya'll", 
                                "y'all",
                                "warden"),
                       lexicon = "custom"))

# tidy the text
y.tidy <- y %>% 
  unnest_tokens(word,y) %>% 
  anti_join(custom_stop_words)

# list of top 20 used words, sorted
y.tidy %>% 
  count(word, sort = TRUE) %>% 
  slice(1:20)

# plot of above list of top used words
y.tidy %>% 
  count(word, sort = TRUE) %>% 
  filter(n>80) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# wordcloud of above list of top used words
pal <- brewer.pal(8,"Dark2")
y.tidy %>% 
  count(word) %>% 
  with(wordcloud(word,n,
                 max.words=80,
                 min.freq=20,
                 colors=pal,
                 random.order = F,
                 random.color = F,
                 scale=c(3,.5)
                 ))

# Comparison wordcloud of top used words according to (bing) sentiment
## negative words likely taken grossly out of context in this case
y.tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 80
                   # scale=c(3,.2)
                   )
## Word contributions to sentiment
# y.tidy %>% 
#   inner_join(get_sentiments("bing")) %>% 
#   count(word, sentiment, sort=TRUE) %>% 
#   filter(n>20) %>% 
#   mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n, fill = sentiment)) +
#   geom_col() +
#   coord_flip() +
#   labs(y = "Contribution to sentiment")

## We can try different sentiment dictionaries
# y.tidy %>%
#   inner_join(get_sentiments("nrc")) %>% 
#   count(word, sentiment, sort = TRUE) %>%
#   acast(word ~ sentiment, value.var = "n", fill = 0) %>%
# 
# y.tidy %>%
#   inner_join(get_sentiments("loughran")) 

# comparison of last statements between races?
## select statement and race columns
x.1 <- x %>% 
  select(Race, LastStatement)
## unnested words used per race
x.2 <- x.1 %>% 
  unnest_tokens(word,LastStatement) %>% 
  count(Race,word,sort=T) %>% 
  ungroup()
## total words used per race
x.3 <- x.2 %>% 
  group_by(Race) %>% 
  summarise(total=sum(n))
## each word used compared to total used per race
x.4 <- left_join(x.2,x.3)

## common words like i,you, to, etc appear very frequently of course
ggplot(x.4,aes(n/total,fill=Race))+
  geom_histogram(show.legend=FALSE)+
  # xlim(NA,0.0009)+
  facet_wrap(~Race,scales="free_y")

x.5 <- x.4 %>% 
  mutate(rank=row_number(),
         `term frequency` =n/total)
x.5 %>% 
  ggplot(aes(rank,`term frequency`,color=Race))+
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()

## Find common words in the corpus

x.6 <- x.4 %>% 
  bind_tf_idf(word,Race,n)

x.6 %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

x.6 %>% 
  # filter(Race!="Other") %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word=factor(word,levels=rev(unique(word)))) %>% 
  group_by(Race) %>% 
  top_n(25,tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf,fill=Race))+
  geom_col(show.legend=F)+
  labs(x=NULL,y="tf-df")+
  facet_wrap(~Race,scale="free")+
  coord_flip()

# bigrams
x.bigrams <- x.1 %>% 
  unnest_tokens(bigram,LastStatement,token="ngrams",n=2)

x.bigrams %>% 
  count(bigram,sort=T)

x.bigrams.separated <- x.bigrams %>% 
  separate(bigram, c("word1","word2"),sep=" ")

x.bigrams.filtered <- x.bigrams.separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

x.bigram.counts <- x.bigrams.filtered %>% 
  count(word1,word2,sort=T)

x.bigrams.united <- x.bigrams.filtered %>% 
  unite(bigram,word1,word2,sep=" ")

x.bigrams.filtered %>% 
  filter(word1 == "polunsky") %>% 
  count(Race, word1,sort=T)

x.bigram.tf.idf <- x.bigrams.united %>% 
  count(Race,bigram) %>% 
  bind_tf_idf(bigram,Race,n) %>% 
  arrange(desc(tf_idf))

x.bigram.tf.idf %>% 
  filter(Race!="Other") %>% 
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

x %>% 
  filter(str_detect(LastStatement,"innocent")) %>% 
  select(LastStatement)
  
# trigrams
x.trigrams <- x.1 %>% 
  unnest_tokens(trigram,LastStatement,token="ngrams",n=3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  # filter(!word1 %in% stop_words$word,
  #        !word2 %in% stop_words$word,
  #        !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


x.bigrams.separated %>% 
  filter(word1=="not") %>% 
  count(word1,word2,sort=T)

AFINN <- get_sentiments("afinn")
x.not.words <- x.bigrams.separated %>% 
  filter(word1=="not") %>% 
  inner_join(AFINN,by=c(word2="word")) %>% 
  count(word2,score,sort=T) %>% 
  ungroup()

x.not.words %>% 
  mutate(contribution=n*score) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2=reorder(word2,contribution)) %>% 
  ggplot(aes(word2,n*score,fill=n*score>0))+
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

negation_words <- c("not", "no", "never", "without")

negated_words <- x.bigrams.separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()


library(igraph)
library(ggraph)
x.bigram.counts

x.bigram.graph <- x.bigram.counts %>% 
  filter(n>4) %>% 
  graph_from_data_frame()

ggraph(x.bigram.graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(x.bigram.graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


x.1 %>% 
  filter(Race=="Black")

