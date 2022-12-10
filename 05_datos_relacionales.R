# 5 Datos relacionales

library(tidyverse)
library(dplyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)
library(nycflights13)

# Es raro que un análisis de datos involucre solo una sola tabla de datos. 
# Por lo general, tiene muchas tablas de datos y debe combinarlas para responder 
# las preguntas que le interesan. En conjunto, varias tablas de datos se denominan 
# datos relacionales porque son las relaciones, no solo los conjuntos de datos
# individuales, las que son importantes.

# Para trabajar con datos relacionales necesitas verbos que funcionen con pares de tablas.
# Hay tres familias de verbos diseñados para trabajar con datos relacionales:
# 
# MUTATING JOINS  
# Combinaciones mutantes , que agregan nuevas variables a un marco de datos a partir
# de observaciones coincidentes en otro.
# 
# FILTERING JOINS
# Combinaciones de filtrado, 
# que filtran las observaciones de un marco de datos en función de si coinciden o 
# no con una observación en la otra tabla.
# 
# SET OPERATIONS
# Operaciones de conjunto , que tratan las observaciones 
# como si fueran elementos de conjunto.

data_airlines <- airlines
data_airports <- airports
data_planes <- planes
data_weather <- weather 
data_flights <- flights
# PRIMARY KEY
# Una clave principal: identifica de forma única una observación en su propia tabla.
# Por ejemplo, planes$tailnumes una clave principal porque identifica de forma única
# cada plano de la planestabla.
# 
# FOREIGN KEY
# Una clave externa: identifica de forma única una observación en otra tabla. 
# Por ejemplo, flights$tailnumes una clave foránea porque aparece en la flights 
# tabla donde hace coincidir cada vuelo con un único avión.


data_planes %>% 
  count(tailnum)
  

data_weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)
       
data_flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

data_flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)


# SURROGATE KEY 
# Cuando comencé a trabajar con estos datos, había asumido ingenuamente que cada 
# número de vuelo solo se usaría una vez al día: eso haría mucho más fácil comunicar
# problemas con un vuelo específico. ¡Desafortunadamente, ese no es el caso! Si una
# tabla carece de clave principal, a veces es útil agregar una con mutate()y row_number().
# Eso hace que sea más fácil hacer coincidir las observaciones si ha realizado algún 
# filtrado y desea volver a verificar con los datos originales.
# Esto se llama clave sustituta.

# MUTATING JOINS  

data_flights_2 <- data_flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

data_flights_2 %>% 
  select(-origin, -dest) %>% 
  left_join(data_airlines, by = "carrier")

data_flights_2 %>% 
  select(-origin, -dest) %>% 
  mutate(name = data_airlines$name[match(carrier, data_airlines$carrier)])

# comprención de las uniones

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x %>% 
  inner_join(y, by = "key")

# NOTA: La propiedad más importante de una combinación interna es que las filas no
# coincidentes no se incluyen en el resultado. Esto significa que, por lo general,
# las uniones internas no suelen ser apropiadas para su uso en el análisis porque 
# es demasiado fácil perder las observaciones.

# OUTER JOINS 
# Uniones externas

# LEFT_JOIN Una combinación izquierda mantiene todas las observaciones en formato x.
# RIGHT_JOIN Una unión a la derecha mantiene todas las observaciones en y.
# FULL_JOIN Una combinación completa mantiene todas las observaciones en x y y.

x %>% 
  left_join(y, by = "key")

x %>% 
  right_join(y, by = "key")

x %>% 
  full_join(y, by = "key")


# NOTA: La combinación más utilizada es la combinación izquierda: la usa cada vez
# que busca datos adicionales en otra tabla, porque conserva las observaciones 
# originales incluso cuando no hay una coincidencia. La combinación izquierda debe
# ser su combinación predeterminada: utilícela a menos que tenga una razón sólida 
# para preferir una de las otras.

# DUPLICATE KEYS
# llaves duplicadas


x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

x %>% 
  left_join(y, by = "key")


x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

x %>% 
  left_join(y, by = "key")


# Definición de columnas clave

data_flights_2 %>% 
  left_join(data_weather)

data_flights_2 %>% 
  left_join(data_planes, by = "tailnum")

data_flights_2 %>% 
  left_join(data_airports, c("dest" = "faa"))


data_airports %>% 
  semi_join(data_flights, c("faa" = "dest")) %>% 
  ggplot(aes(
    lon,
    lat,
    size = alt
  ))+
  borders("state")+
  geom_point()+
  coord_quickmap()




# dplyr	            unir
# inner_join(x, y)	merge(x, y)
# left_join(x, y)	  merge(x, y, all.x = TRUE)
# right_join(x, y)	merge(x, y, all.y = TRUE),
# full_join(x, y)	  merge(x, y, all.x = TRUE, all.y = TRUE)


# dplyr	                      sql
# inner_join(x, y, by = "z")	SELECT * FROM x INNER JOIN y USING (z)
# left_join(x, y, by = "z")	  SELECT * FROM x LEFT OUTER JOIN y USING (z)
# right_join(x, y, by = "z")	SELECT * FROM x RIGHT OUTER JOIN y USING (z)
# full_join(x, y, by = "z")	  SELECT * FROM x FULL OUTER JOIN y USING (z)


# filtrado de uniones

# semi_join(x, y) mantiene todas las observaciones en x que tienen una coincidencia en y.
# anti_join(x, y) elimina todas las observaciones x que tienen una coincidencia en y

top_dest <- data_flights %>% 
  count(dest, sort = TRUE) %>% 
  head(20)

top_dest %>% 
  ggplot(aes(
    n,
    dest
  ))+
  geom_point()

data_flights %>% 
  filter(dest %in% top_dest$dest)


data_flights %>% 
  semi_join(top_dest)


data_flights %>% 
  anti_join(data_planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)
