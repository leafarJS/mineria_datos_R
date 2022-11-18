library(tidyverse)
library(scales)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(gutenbergr)
#FORMATO DE TEXTO ORDENADO

# Cada variables es una columna
# Cada observacion en una fila
# Cada tipo de unidad de observacion es una tabla

#1.1 Contrastando texto ordenado con otras estructuras de datos

#1.2 La unnest_tokensfunción

texto <- c("Ved! En una noche de gala,",
           "En los tardíos años desolados.",
           "Una hueste de ángeles alados,",
           "Envueltos en velos y ahogados en lágrimas...")

texto_df <- tibble(line = 1: 4, text = texto)

texto_df

# Un token es una unidad significativa de texto, generalmente una palabra, que nos
# interesa usar para un análisis más detallado, y la tokenización es el proceso de dividir el texto en tokens.

texto_df %>% 
  unnest_tokens(word, text)


#1.3 Poner en orden las obras de Jane Austen
libro_orignal <- austen_books() %>% 
  group_by(book) %>% 
  mutate(numero_linea =  row_number(),
         capitulo = cumsum(str_detect(text,
                                      regex("^capitulo[\\divxlc]",
                                            ingore_case = TRUE)))) %>% 
  ungroup()

libro_orignal

tidy_libro <- libro_orignal %>% 
  unnest_tokens(word, text)

tidy_libro


data("stop_words")
# El stop_wordsconjunto de datos del paquete tidytext contiene palabras vacías de
# tres léxicos. Podemos usarlos todos juntos, como lo hemos hecho aquí, o filter()
# usar solo un conjunto de palabras vacías si eso es más apropiado para un determinado análisis


tidy_libro <- tidy_libro %>% 
  anti_join(stop_words)

tidy_libro %>% 
  count(word, sort = TRUE) %>% 
  View()


tidy_libro %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(
    x = n,
    y = word
  ))+
  geom_col(aes(fill = word))+
  labs(y= NULL)+
  theme_minimal()+
  theme(legend.position = "none")


#1.4 El paquete gutenbergr
# Para obtener más información sobre gutenbergr, consulte la documentación del 
# paquete en rOpenSci , donde se encuentra uno de los paquetes de rOpenSci para el acceso a datos.

#1.5 Frecuencias de palabras
devtools::install_github("ropensci/gutenbergr")
library(gutenbergr)

hgwells <- gutenberg_download(c(35,36,5230,159))

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_hgwells %>% 
  count(word, sort = TRUE) %>% 
  View()


# download The Count of Monte Cristo
dummas <- gutenberg_download(1184)


tidy_dummas <- dummas %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) 

tidy_dummas %>% 
  count(word, sort = TRUE) %>% 
  View()

tidy_dummas %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(
    x = n,
    y = word
  ))+
  geom_col(aes(fill = word))+
  labs(y= NULL)+
  theme_minimal()+
  theme(legend.position = "none")


#unir los libros analizados
frecuencia <- bind_rows(mutate(tidy_dummas, author = "Alejandro Dummas"),
                        mutate(tidy_hgwells, author = "H.G. Wells"),
                        mutate(tidy_libro, author = "Jane Austen")) %>% 
 mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proporcion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proporcion) %>% 
  view()

frecuencia <- bind_rows(mutate(tidy_dummas, author = "Alejandro Dummas"),
                        mutate(tidy_hgwells, author = "H.G. Wells"),
                        mutate(tidy_libro, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proporcion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proporcion) %>% 
  pivot_longer(`Alejandro Dummas`:`H.G. Wells`,
               names_to = "author", values_to = "proporcion") %>% 
  view()


frecuencia %>% 
  ggplot(aes(
    x = proporcion,
    y = `Jane Austen`,
    color = abs(`Jane Austen` - proporcion)
  ))+
  geom_abline(color = "gray40", lty = 2)+
  geom_jitter(alpha = 0.1,
              size = 2.5,
              width = 0.3,
              height = 0.3)+
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)+
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  scale_color_gradient(limits = c(0, 0.001),
                       low  = "darkslategray4",
                       high = "gray75")+
  facet_wrap(~author, ncol = 2)+
  theme(legend.position = "none")+
  labs(y = "Jane Austen", x = NULL)


# ANALISIS DE LA CORRELACION QUE EXITE ENTRE AUTORES

cor.test(data = frecuencia[frecuencia$author == "Alejandro Dummas",],
         ~ proporcion + `Jane Austen`)

cor.test(data = frecuencia[frecuencia$author == "H.G. Wells",],
         ~ proporcion + `Jane Austen`)
