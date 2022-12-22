library(tidyverse)
library(tidytext)
library(ggplot2)
library(widyr)
library(lubridate)
theme_set(theme_minimal())

# funciones

df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

(df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))


x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

#mejorando el codigo
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))
rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))

#simplificar los pasos
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

x <- c(1:10, Inf)
rescale01(x)


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)


corto_circuito <-function(x, y, op) {
    switch(op,
     plus = x + y,
     minus = x - y,
     times = x * y,
     divide = x / y,
     stop("Unknown op!")
   )
}
corto_circuito(10,30, "plus")
corto_circuito(10,30, "divide")


y <- 10
x <- if (y < 20) "Too low" else "Too high"
x


# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_ci(x) # 0.4976111 0.6099594
mean_ci(x, conf = 0.99) # 0.4799599 0.6276105

# Elección de nombres 
 
# x, y, z: vectores.
# w: un vector de pesos.
# df: un marco de datos.
# i, j: índices numéricos (típicamente filas y columnas).
# n: longitud, o número de filas.
# p: número de columnas.


# ¿Qué pasa si xy wno tienen la misma longitud?

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

wt_mean(1:6, 1:3)
wt_mean(1:10, 2:11)


wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")# Error in wt_mean(1:6, 6:1, na.rm = "foo"): is.logical(na.rm) is not TRUE
wt_mean(1:6, 1:6, na.rm = TRUE)
wt_mean(5:10, 5:10)


sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)# 55
stringr::str_c("a", "b", "c", "d", "e", "f")# "abcdef"


commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10]) # "a, b, c, d, e, f, g, h, i, j"

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")# Important output -------------
