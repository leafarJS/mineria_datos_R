#2 Análisis de sentimiento con datos ordenados

devtools::install_github("ropensci/gutenbergr")
library(gutenbergr)
library(janeaustenr)
library(dplyr)
library(stringr)
library(ggplot2)



dummas <- gutenberg_download(1184) %>% 
  view()

data("stop_words")
 tidy_dummas <- dummas %>% 
 unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
   view()
  

 tidy_count <-tidy_dummas %>% 
   count(word, sort = TRUE) 

  
bing_sentimento <- get_sentiments("bing") %>% 
    filter(sentiment == "negative")

bing_sentimento_1 <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

tidy_count %>% 
  select(word) %>% 
  inner_join(bing_sentimento) %>% 
  count(word, sort = TRUE) %>% 
  View()

########################################################


tidy_austen <- austen_books() %>%
  group_by(book) %>%
  mutate(
    numero_linea = row_number(),
    capitulo = cumsum(str_detect(text, 
                                regex("^capitulo[\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


bad <- tidy_austen %>% 
  filter(book == "Emma") %>% 
  inner_join(bing_sentimento) %>% 
  count(word, sort = TRUE)


good <- tidy_austen %>% 
  filter(book == "Emma") %>% 
  inner_join(bing_sentimento_1) %>% 
  count(word, sort = TRUE) 


jane_austen_sentimientos <-tidy_austen %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(book, index = numero_linea %/% 80, sentiment) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  view()


jane_austen_sentimientos %>% 
  ggplot(aes(
    x = index,
    y = sentiment,
    fill = book
  ))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~book, 
             ncol = 2,
             scales = "free_x")

#Palabras positivas y negativas más comunes

bing_palabras_contadas <- tidy_austen %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup() %>% 
  View()

bing_palabras_contadas %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(
    x = n,
    y = word, 
    fill = sentiment
  ))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x = "Contribuyen al sentimiento",
       y = "")


install.packages(c("wordcloud","tm"),repos="http://cran.r-project.org")
library(wordcloud)
library(tm)

tidy_austen %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)

tidy_austen %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0 ) %>% 
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 100)
