library(tidyverse)
library(magrittr)
library(pryr)
theme_set(theme_minimal())
# Pipas o tuberias 

# pryr::object_size()da la memoria ocupada por todos sus argumentos. Los 
# resultados parecen contradictorios al principio

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat = price / carat)

pryr::object_size(diamonds)# 3.46 MB
pryr::object_size(diamonds2)# 3.89 MB
pryr::object_size(diamonds, diamonds2) # 3.89 MB

diamonds$carat[1] <- NA
pryr::object_size(diamonds)#  3.46 MB
pryr::object_size(diamonds2)#  3.89 MB
pryr::object_size(diamonds, diamonds2) # 4.32 MB


# Esto significa que la tubería no funcionará para dos clases de funciones:
 
# Funciones que utilizan el entorno actual. Por ejemplo, assign() creará
# una nueva variable con el nombre dado en el entorno actual:

assign("x", 10)
x # 10

"x" %>% assign(100)
x#  10

# Si desea utilizar la asignación con la canalización, 
# debe ser explícito sobre el entorno:

env <- environment()
"x" %>% assign(100, envir = env)
x # 100


# Funciones que usan evaluación perezosa. En R, los argumentos de la función 
# solo se calculan cuando la función los usa, no antes de llamar a la función. 
# La tubería calcula cada elemento por turno, por lo que no puede confiar en
# este comportamiento.

# Un lugar donde esto es un problema es tryCatch(), que le permite capturar
# y manejar errores: Hay una clase relativamente amplia de funciones con este 
# comportamiento, incluidas try(), suppressMessages() y suppressWarnings() en base R.

tryCatch(stop("!"), error = function(e) "An error")# "An error"

stop("!") %>% 
  tryCatch(error = function(e) "An error")# "An error"

# Para solucionar este problema, puede utilizar el tubo en "T". %T>%funciona así
# %>% excepto que devuelve el lado izquierdo en lugar del lado derecho. Se llama 
# "tee" porque es como un tubo en forma de T literal.

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()
#>  NULL

rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()
#>  num [1:50, 1:2] -0.387 -0.785 -1.057 -0.796 -1.756 ...


mtcars %$%
  cor(disp, mpg)# -0.8475514

mtcars <- mtcars %>% 
  transform(cyl = cyl * 2)

mtcars %<>% 
  transform(cyl = cyl * 8) 
