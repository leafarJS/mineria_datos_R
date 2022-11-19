# 3 Analisis de frecuencias de palabras y documentos : tf:idf
devtools::install_github("ropensci/gutenbergr")
library(gutenbergr)
library(stringr)
library(janeaustenr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(forcats)


#3.1 Frecuencia de términos en las novelas de Jane Austen

palabras.libros <- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book, word, sort = TRUE)

palabras.totales <- palabras.libros %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

palabras.libros <- left_join(palabras.libros, palabras.totales)

str(palabras.libros)

palabras.libros %>% 
  ggplot(aes(
    n/total,
    fill = book
  ))+
  geom_histogram(show.legend = FALSE)+
  xlim(NA, 0.0009)+
  facet_wrap(~book, ncol = 2, scales = "free_y")+
  labs(x = "Distribución de frecuencia de términos en las novelas de Jane Austen")

#3.2 Ley de Zipf

# La ley de Zipf establece que la frecuencia con la que aparece una palabra es
# inversamente proporcional a su rango.

frecuencia.por.rango <- palabras.libros %>% 
  group_by(book) %>% 
  mutate(rango = row_number(),
         `term frequency` = round(n/total,2)) %>% 
  ungroup()

frecuencia.por.rango %>% 
  ggplot(aes(
    rango,
    `term frequency`,
    color = book
  ))+
  geom_line(size = 1.1,
            alpha = 0.8, 
            show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Ley de Zipf para las novelas de Jane Austen")

frecuencia.por.rango %>% 
  ggplot(aes(
    rango,
    `term frequency`,
    color = book
  ))+
  geom_line(size = 1.1,
            alpha = 0.8, 
            show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~book)+
  labs(x = "Ley de Zipf para las novelas de Jane Austen")


subconjunto.rango <- frecuencia.por.rango %>% 
  filter(rango < 500,
         rango > 100) 


frecuencia.por.rango %>% 
  ggplot(aes(
    rango,
    `term frequency`,
    color = book
  ))+
  geom_abline(intercept = -0.62,
              slope = -1.1,
              color = "red", 
              linetype = 1)+
  geom_line(size = 1.1,
            alpha = 0.8, 
            show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~book)+
  labs(x = "Ley de Zipf para las novelas de Jane Austen")


#3.3 La bind_tf_idf()función


libros.tf_idf <- palabras.libros %>% 
  bind_tf_idf(word, book, n) 

libros.tf_idf %>% 
  select(-total) %>% 
  arrange(desc(tf_idf)) 

libros.tf_idf %>% 
  group_by(book) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>% 
  ggplot(aes(
    tf_idf,
    fct_reorder(word, tf_idf),
    fill = book
  ))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~book, ncol = 2, scales = "free")+
  labs(title = "Palabras tf-idf más altas en cada novela de Jane Austen",
    x = "tf_idf", y = NULL)


#3.4 Un corpus de textos de física

# Discurso sobre cuerpos flotantes de Galileo Galilei,
# Tratado sobre la luz de Christiaan Huygens,
# Experimentos con corrientes alternas de alto potencial y alta frecuencia de Nikola Tesla y Relatividad:
# La teoría especial y general de Albert Einstein .

fisica <- gutenberg_download(c(37729,14725,13476,30155),
                             meta_fields = "author")

palabras.fisica <- fisica %>% 
  unnest_tokens(word, text) %>% 
  count(author, word, sort = TRUE) 



plot.fisica <- palabras.fisica %>% 
  bind_tf_idf(word, author, n) %>% 
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot.fisica %>% 
  group_by(author) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, tf_idf)) %>% 
  ggplot(aes(
    tf_idf,
    word,
    fill = author
  ))+
  geom_col(show.legend = FALSE)+
  labs(title = "Palabras tf-idf más altas en cada texto de física",x = "tf_idf", y = NULL)+
  facet_wrap(~author, ncol = 2, scales = "free")


fisica %>% 
  filter(str_detect(text, "_k_")) %>% 
  select(text) %>% 
  View()

fisica %>% 
  filter(str_detect(text, "RC")) %>% 
  select(text) %>% 
  View()

palabras.excluidas <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                      "fig", "file", "cg", "cb", "cm",
                                      "ab", "_k", "_k_", "_x"))


palabras.fisica <- anti_join(palabras.fisica, palabras.excluidas,
                             by = "word") 

plot.fisica <- palabras.fisica %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(author) %>% 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))


plot.fisica %>% 
  ggplot(aes(
    tf_idf,
    word, 
    fill = author
  ))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~author, ncol = 2, scales = "free") +
  labs(title = "Palabras tf-idf más altas en textos clásicos de física limpio", x = "tf-idf", y = NULL)
  
