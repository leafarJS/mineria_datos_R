library(tidyverse)

# iteración

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

x <- vector("double", 15)
seq_along(x)
1:length(x)


# media de cada columna de mtcars
output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[[i]] <- median(mtcars[[i]])
}
output


output <- vector("character", ncol(nycflights13::flights))
for (i in seq_along(nycflights13::flights)) {
  output[[i]] <- str(nycflights13::flights[[i]])
}
output

output <- ""
for (i in letters) {
  output <- stringr::str_c(output, i)
}
output

x <- sample(10)
x
output <- 0
for (i in seq_along(x)) {
  sd <- output + (x[i] - mean(x)) ^ 2
}
sd

x <- runif(100)
x
output <- vector("numeric", length(x))
output <- 0
output
for (i in 2:length(x)) {
  output[i] <- output[i - 1] + x[i]
}
output


output <- vector("numeric", length(x))
output[1] <- x[1]
output
for (i in 2:length(x)) {
  output[i] <- output[i - 1] + x[i]
}
output

# modificar un objeto existente

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df$a

scala <- function(x) {
  rango <- range(x, na.rm = TRUE)
  (x - rango[1]) / (rango[2] - rango[1])
}
scala(df$a)

for (i in seq_along(df)) {
  df[[i]] <- scala(df[[i]])
}
df

# patrones de bucle

x <- 1:10
x

results <- vector("list", length(x))
results

names(results) <- names(x)
names

for (i in seq_along(x)) {
  name <- names(x)[[i]]
  print(name)
  value <- x[[i]]
  print(value)
}

x <- 1:10
x
result <- vector("double", length(x))
for (i in 1:length(x)) {
  result <- x[i] * 2 
  print(result)
}
result
