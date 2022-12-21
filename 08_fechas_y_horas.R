library(tidyverse)
library(dplyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)
theme_set(theme_minimal())

# Fechas y Horarios 

library(lubridate)
library(nycflights13)

# creación de fecha/hora

# paquete hms para manejo de horas

#fecha y hora actual
today()
now()

# desde cadenas
ymd("2022-12-21")
mdy("January 31st 2023")
dmy("31-Jan-2017")

ymd(20221221)


ymd_hms("2022-12-21 09:31:42")

mdy_hm("12/21/2022 09:30")

ymd(20221221, tz = "UTC")

# de componentes individuales
view(flights)

flights %>% 
  select(year, month, day, hour, minute)

# Para crear una fecha/hora a partir de este tipo de entrada
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))


make_datetime_100  <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time")) %>% 
  view()

flights_dt %>% 
  ggplot(aes(
    dep_time
  ))+
  geom_freqpoly()

flights_dt %>% 
  ggplot(aes(
    dep_time
  ))+
  geom_freqpoly(binwidth = 86400) #86400 seconds = 1 day


flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(
    dep_time
  ))+
  geom_freqpoly(binwidth = 600) #600 seconds = 10 minutes

# De otros tipos
# Es posible que desee cambiar entre una fecha y hora y una fecha.

as_datetime(today())
as_date(now())

# Si el desplazamiento está en segundos, use as_datetime(); si es en días, use as_date().
as_datetime(60*60*10)
as_date(365*10+2)

ymd(c("2010-10-10", "bananas")) # failed to parse. 

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14"

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

# Componentes de fecha y hora
# Obtener componentes

datetime <- ymd_hms("2022-12-21 12:42:00")
year(datetime)
month(datetime)
mday(datetime)

yday(datetime)
wday(datetime)

month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

#ver que dia salen mas vuelos
flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(
    wday
  ))+
  geom_bar()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) %>% 
  view()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) %>% 
  ggplot(aes(
    minute,
    avg_delay
  ))+
  geom_line()+
  expand_limits(y = 0)


sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) %>% 
  view()

sched_dep %>% 
  ggplot(aes(
    minute,
    avg_delay
  ))+
  geom_line()

sched_dep %>% 
 ggplot(aes(
    minute,
    n
  ))+
  geom_line()

# Redondeo
# Un enfoque alternativo para trazar componentes individuales es redondear
# la fecha a una unidad de tiempo cercana, con floor_date(), round_date() 
# y ceiling_date().


flights_dt %>% 
  count(week = floor_date(dep_time, "week"))

flights_dt %>% 
  count(week = floor_date(dep_time, "month"))

flights_dt %>% 
  count(week = floor_date(dep_time, "year"))


flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(
    week,
    n
  ))+
  geom_line()

# Componentes de ajuste
(datetime <- ymd_hms("2022-12-21 13:07:00"))

year(datetime) <- 2020
datetime

month(datetime) <- 01
datetime

hour(datetime) <- hour(datetime) + 1
datetime

# modificación total 
update(
  datetime,
  year = 1977,
  month = 2,
  mday = 3,
  hour = 2
       )

# si nos pasamos de numero de dias en el mes
ymd("2022-02-01") %>% 
  update(mday = 30)

ymd("2022-02-01") %>% 
  update(hour = 400)

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  view()

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(
    dep_hour
  ))+
  geom_freqpoly(binwidth = 300)

# lapsos de tiempo

# A continuación, aprenderá cómo funciona la aritmética con fechas, incluidas 
# la resta, la suma y la división. En el camino, aprenderá acerca de tres clases
# importantes que representan períodos de tiempo:
 
# Duraciones = que representan un número exacto de segundos.
# períodos = que representan unidades humanas como semanas y meses.
# intervalos = que representan un punto inicial y final.

# duraciones

# En R, cuando restas dos fechas, obtienes un objeto difftime:
h_age <- today() - ymd(19770203)
h_age

as.duration(h_age)

# Las duraciones vienen con un montón de constructores convenientes:

dseconds(15)
dminutes(55)
dhours(12)
dhours(c(1,3,6,12,24))
ddays(0:5)
dweeks(3)
dyears(1)

dyears(1) * 45

# operaciones

4 * dyears(2)
dyears(2) +  dweeks(18) + dhours(13.5)

tomorrow <- today() + ddays(4)
tomorrow

last_year <- today() - dyears(45)
last_year


one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")

one_pm # "2016-03-12 13:00:00 EST"
one_pm + ddays(1) # "2016-03-13 14:00:00 EDT"

# ¿Por qué un día después de la 1:00 p. m. del 12 de marzo, las 2:00 p. m. del
# 13 de marzo? Si observa detenidamente la fecha, también puede notar que las 
# zonas horarias han cambiado. Debido al horario de verano, el 12 de marzo solo
# tiene 23 horas, por lo que si agregamos un día completo en segundos, obtenemos 
# una hora diferente.

# Periodos

# Para resolver este problema, lubridate proporciona períodos . Los períodos son
# lapsos de tiempo, pero no tienen una duración fija en segundos, sino que funcionan
# con tiempos "humanos", como días y meses. Eso les permite trabajar de una manera
# más intuitiva:

one_pm
one_pm + days(1)

seconds(15)
minutes(10)
hours(c(1,3,6,12,24))
days(7)
months(1:6)
weeks(3)
years(1)


10 * (months(6) + days(15))

10 * (months(6) + days(15)) + days(50) + hours(25) + minutes(12) + seconds(32)


# A leap year
ymd("2016-01-01") + dyears(1)# "2016-12-31 06:00:00 UTC"
ymd("2016-01-01") + years(1)#  "2017-01-01"

# Daylight Savings Time
one_pm + ddays(1) # "2016-03-13 14:00:00 EDT"
one_pm + days(1) #  "2016-03-13 13:00:00 EDT"


flights_dt %>% 
  filter(arr_time < dep_time) %>% 
  view()

# Estos son vuelos nocturnos. Utilizamos la misma información de fecha tanto para
# la hora de salida como para la de llegada, pero estos vuelos llegaron al día 
# siguiente. Podemos arreglar esto sumando days(1)a la hora de llegada de cada 
# vuelo nocturno.

flights_dt_1 <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time, # false
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1 )
  ) %>% 
  view()
# Ahora todos nuestros vuelos obedecen las leyes de la física.
flights_dt_1 %>% 
  filter(overnight, arr_time < dep_time)


# Intervalos 
years(1) / days(1)

# Si quieres una medida más precisa, tendrás que usar un intervalo . Un intervalo
# es una duración con un punto de partida: eso lo hace preciso para que puedas 
# determinar exactamente cuánto dura:

next_year <- today() + years(1)
next_year

(today() %--% next_year) / ddays(1)

# Para averiguar cuántos períodos caen en un intervalo, 
#debe usar la división de enteros:

(today() %--% next_year) %/% ddays(1)


(today() %--% (today() + years(1))) / months(12)

# Zonas Horarias 
# zona horaria actual segun ubicación en R
Sys.timezone()
# nombres de zonas horarias
OlsonNames()

length(OlsonNames())

(x1 <- ymd_hms("2022-12-01 12:00:00", tz = "America/New_York")) # "2022-12-01 12:00:00 EST"
(x2 <- ymd_hms("2022-12-01 18:00:00", tz = "Europe/Copenhagen")) # "2022-12-01 18:00:00 CET"
(x3 <- ymd_hms("2022-12-02 04:00:00", tz = "America/Caracas")) # "2022-12-02 04:00:00 -04"

x1 - x2
x2 - x3

x4 <- c(x1,x2,x3)
x4
x4a <-with_tz(x4, tzone = "America/Caracas")
x4a

x4a - x4

x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b

x4b - x4
