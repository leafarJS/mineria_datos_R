library(tidyverse)
library(dplyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)
library(nycflights13)

# 06 Strings

# comillas


string_1 <- "Esto es un String en R"
string_2 <- "Esto es un 'String' en R"
string_3 <- 'Esto es un "'"string"'" en R'

string_1
string_2
string_3


x <- c("\"", "\\")
x
writeLines(x)


# longitud de la cadena

x <- "esto es una frase"
length(x)

str_length(c("a", "ab", "abc", "abcd", "abcde", NA))


# conbinación de cadenas

str_c("jor", "ge")
str_c("ra", "fa", "el")
str_c("jorge", "rafael", sep = ",")

# NA

 x <- c("abc", NA)
str_c("|-", x, "-|") # "|-abc-|" NA  
str_c("|-", str_replace_na(x), "-|") # "|-abc-|" "|-NA-|" 

# if

name <- "Hadley"
time_of_day <- "morning"
brithday <- TRUE

str_c("Good ", time_of_day, " ", name, if (brithday) "and HAPPY BIRTHDAY", ".")

# COLLAPSE

str_c(c("a", "la", "verga"), collapse = ", ")

# Cadena de sub consjuntos 
x <- c("Apple", "Banana", "Pear", "12345", "67890")
str_sub(x, 1, 3)
str_sub(x, -3, -1)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x,1,1))
x

str_sub(x, 1, 1) <- str_to_upper(str_sub(x,1,1))
x

# Locales 

x <- c("uno", "dos", "tres")
str_sub(x, 1, 1) <- str_to_title(str_sub(x,1,1))
x

x <- c("  auto  ", "  radio", "winbledon   ")

str_sort(x, locale = "es")
str_sort(x, locale = "en")


paste(1:12)
paste0(1:12)

(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
# "1st"  "2nd"  "3rd"  "4th"  "5th"  "6th"  "7th"  "8th"  "9th"  "10th" "11th" "12th"

paste(month.abb, "es el", nth, "mes del año.")
paste(month.abb, letters)


paste(month.abb, nth, sep = ": ", colapso = "; ") 


str_wrap(x) # Envuelva las palabras en párrafos, minimizando la "desigualdad" de las líneas (es decir, la variación en la longitud de la línea) utilizando el algoritmo de Knuth-Plass.
str_trim(x) # str_trim()elimina los espacios en blanco al principio y al final de la cadena; str_squish() elimina los espacios en blanco al principio y al final, y reemplaza todos los espacios en blanco internos con un solo espacio.


# Coincidencia de patrones con expresiones regulares




