# 4 Relaciones entre palabras: n-gramas y correlaciones

# 4.1 Tokenización por n-gram
library(tidyverse)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)


bigrams <- austen_books() %>% 
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>% 
  filter(!is.na(bigram)) 

# Tenga en cuenta que estos bigramas se superponen: "sentido y" es una ficha,
# mientras que "y sensibilidad" es otra.
# 4.1.1 Conteo y filtrado de n-gramas

bigrams %>% 
  count(bigram, sort = TRUE) 
# contiene palabras comunes que no nos interesa

bigrams.separated <- bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams.separated

bigrams.filtered <- bigrams.separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

bigrams.filtered


# new bigram counts:
bigram.counts <- bigrams.filtered %>% 
  count(word1, word2, sort = TRUE)

bigram.counts


bigram.united <- bigrams.filtered %>% 
  unite(bigram, word1, word2, sep = " ")

bigram.united


t#trigrama
austen_books() %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  filter(!is.na(trigram)) %>% 
  separate(trigram, c("text1", "text2", "text3"), sep = " ") %>% 
  filter(!text1 %in% stop_words$word,
         !text2 %in% stop_words$word,
         !text3 %in% stop_words$word) %>% 
  count(text1, text2, text3, sort = TRUE)


# 4.1.2 Análisis de bigramas
# Este formato de un bigrama por fila es útil para los análisis exploratorios del
# texto. Como un ejemplo simple, nos pueden interesar las “calles” más comunes 
# mencionadas en cada libro:

bigrams.filtered %>% 
  filter(word2 == "street") %>% 
  count(book, word1, sort = TRUE) 


bigram.tf_idf <- bigram.united %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf)) 

bigram.tf_idf %>% 
  group_by(book) %>% 
  head(40) %>% 
  mutate(bigram  = fct_reorder(bigram, n)) %>% 
  ggplot(aes(
    tf_idf,
    bigram,
    fill = book
  ))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~book, ncol = 2, scales = "free")+
  theme_minimal()

# 4.1.3 Uso de bigramas para proporcionar contexto en el análisis de sentimientos

bigrams.separated %>% 
  filter(word1 == "not") %>% 
  count(word1, word2, sort = TRUE)


bigram.graph <- bigram.counts %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()

bigram.graph


ggplot(bigram.graph, layout = 'fr') %>% 
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)




count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
} 

library(gutenbergr)
kjv <- gutenberg_download(10)

kjv_bigrams <- kjv %>%
  count_bigrams()

# filter out rare combinations, as well as digits
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()


# 4.2 Contar y correlacionar pares de palabras con el paquete widyr
austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words



# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

word_pairs %>%
  filter(item1 == "darcy") %>% 
  head(20) %>% 
  mutate(item2 = fct_reorder(item2, n)) %>% 
  ggplot(aes(
    n,
    item2
  ))+
  geom_col()


#4.2.2 Correlación por pares

word.cors <-austen_section_words %>% 
  group_by(word) %>% 
  filter(n() >= 20) %>% 
  pairwise_cor(word, section, sort = TRUE) 

word.cors %>% 
  filter(item1 == "pounds")%>% 
  view()

word.cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word.cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
