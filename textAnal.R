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

setwd("C:/Users/e/Documents/R/inmate.last.words")
y <- read_csv("data/Texas Last Statement - CSV.csv")
  
# y <- data_frame(y$LastStatement)
y <- y$LastStatement
y <- data_frame(y)

y.tidy <- y %>% 
  unnest_tokens(word,y) %>% 
  anti_join(stop_words)


y.tidy %>% 
  count(word, sort = TRUE)

y.tidy %>% 
  count(word, sort = TRUE) %>% 
  filter(n>80) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# top words
pal <- brewer.pal(8,"Dark2")
y.tidy %>% 
  count(word) %>% 
  with(wordcloud(word,n,
                 max.words=100,
                 min.freq=20,
                 colors=pal,
                 random.order = F,
                 random.color = F
                 # scale=c(3,.3)
                 ))

## Comparison wordcloud of all text with sentiments
y.tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 80
                   # scale=c(3,.2)
                   )

y.tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort=TRUE) %>% 
  filter(n>20) %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

y.tidy %>%
  inner_join(get_sentiments("nrc")) 
y.tidy %>%
  inner_join(get_sentiments("loughran")) 
