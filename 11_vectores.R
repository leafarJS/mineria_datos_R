library(tidyverse)

# Vectores

#tipo
typeof(letters)
typeof(1:20)

#longitud
x <- list("a", "b", 1:20)
length(x)

# Tipos importantes de vectores atómicos

# logicos
1:10 %% 3 == 0

c(TRUE, FALSE, FALSE, NA)


# numérico
typeof(1)
typeof(1L)
1.5L

x <- sqrt(2) ^ 2
x
x - 2
x

c(-1,0,1) / 0

# Evite usar == para verificar estos otros valores especiales. 
# En su lugar, utilice las funciones auxiliares is.finite(), 
# is.infinite()y is.nan():

x <- c(1,2,1/8,NA, NaN, Inf, -Inf)
is.infinite(x)
is.finite(x)
is.na(x)
is.nan(x)


#carácter

x <- "Esta es una cadena razonablemente larga"
pryr::object_size(x)
y <- rep(x, 1000)
pryr::object_size(y)



# valores faltantes

NA            # logical  NA
NA_integer_   # integer  NA
NA_real_      # double   NA
NA_character_ # character NA

x <- sample(1:10)
dplyr::near(x, 8)


# Uso de vectores atómicos 

# coacción

x <- sample(20,100, replace = TRUE)
x

y <- x > 10
y

sum(y)
mean(y)


# el tipo más complejo siempre gana

typeof(c(TRUE, 1L))
typeof(c(1L,1.5))
typeof(c(1.5, "a"))


# funciones de prueba 

x <- c(1,2,3,"a", NA, NaN, 1.5, 8L, -Inf, Inf, NULL)
is_logical(x)
is_integer(x)
is_double(x)
is_numeric(x)
is_character(x)
is_atomic(x)
is_list(x)
is_vector(x)


# escalares y reglas de reciclaje

sample(10) + 100
runif(10) > 0.5

1:10 + 1:2
1:10 + 1:3 # In 1:10 + 1:3 : longer object length is not a multiple of shorter object length

tibble(x = 1:4, y = 1:2) # error
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))

# Nomenclatura de vectores

c(x = 1, y = 2, z = 3)
purrr::set_names(1:3, c("a", "b", "c"))


# creación de subconjuntos

x <- c("one", "two", "three", "four", "five")
x[c(3,2,5)]
x[c(1,1,5,5,5,2,4)]
x[c(-1,-5)]
x[c(1,-1)] # Error in x[c(1, -1)] : only 0's may be mixed with negative subscripts
x[0]

x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]
x[x %% 2 == 0]


x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]


# vectores recursivos 
x <- list(1,2,3)
x
str(x) # herramienta util

x <- list(a= 1, b = 2, c = 3)
str(x)

x <- list("a", 1L, 1.5, TRUE)
str(x)

x <- list(list(1, 2, "a", FALSE), list(3, 4, -Inf))
str(x)


# visualización de listas 
x <- list(c(1,2), c(3,4))
y <- list(list(1,2), list(3,4))
z <- list(1, list(2, list(3)))


# creacion de subconjuntos 
x <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

str(x[1:2])
str(x[4])

str(x[[1]])
str(x[[4]])

x$a
x$d
x$d[1]


# atributos 
x <- 1:10
attr(x, "greeting")

attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"

attributes(x)


# Así es como se ve una función genérica típica:
as.Date

# La llamada a "UseMethod" significa que esta es una función genérica y
# llamará a un método específico , una función, según la clase del primer argumento.

methods("as.Date")

getS3method("as.Date", "default")
getS3method("as.Date", "numeric")


# vectores aumentados 


# factores 
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
x
typeof(x)
attributes(x)


# fechas y fechas-horas
x <- as.Date("1977-02-03")
# desde el 1 de enero de 1970.
unclass(x)

typeof(x)
attributes(x)

# ("POSIXct" significa "Interfaz de sistema operativo portátil", 
# tiempo de calendario)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
attr(x, "tzone")
typeof(x)
attributes(x)

attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

y <- as.POSIXlt(x)
y
typeof(y)
attributes(y)


# Dado que lubridate proporciona ayudantes para que usted haga esto, no los necesita.
# Siempre es más fácil trabajar con POSIXct, por lo que si descubre que tiene un POSIXlt,
# siempre debe convertirlo a un tiempo de datos regular lubridate::as_date_time().


# tibbles

x <- tibble::tibble(x = 1:5, y = 5:1)
x
typeof(x)
attributes(x)

x <- data.frame(x = 1:5, y = 5:1)
x
typeof(x)
attributes(x)
