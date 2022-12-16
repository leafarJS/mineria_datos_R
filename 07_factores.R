library(tidyverse)
library(dplyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)

# Factores

meses_niveles <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "agt", "sep", "oct", "nov", "dic")
x <- factor(meses_niveles, levels = meses_niveles)
x
sort(x)

gss_cat

gss_cat %>% 
  count(race)

gss_cat %>% 
  ggplot(aes(
    race
  ))+
  geom_bar()

# De forma predeterminada, ggplot2 eliminará los niveles que no tengan ningún 
# valor. Puede obligarlos a mostrarse con:

gss_cat %>% 
  ggplot(aes(
    race
  ))+
  geom_bar()+
  scale_x_discrete(drop = FALSE)

str(gss_cat)

gss_cat %>% 
  count(rincome)

gss_cat %>% 
  count(rincome) %>% 
  filter(!rincome == "Not applicable") %>% 
  mutate(rincome = fct_reorder(rincome, n)) %>% 
  ggplot(aes(
    rincome,
    n,
    fill = rincome
  ))+
  geom_col(show.legend = FALSE)+
  coord_flip()
  


gss_cat %>% 
  count(relig) %>% 
  mutate(relig = fct_reorder(relig, n)) %>% 
  ggplot(aes(
    relig,
    n,
    fill = relig
  ))+
  geom_col(show.legend = FALSE)+
  coord_flip()

# Modificación del orden de los factores

gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  ) %>% 
  mutate(relig = fct_reorder(relig, tvhours)) %>% 
  ggplot(aes(
    tvhours,
    relig
  ))+
  geom_point()

gss_cat %>% 
  group_by(rincome) %>% 
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  ) %>% 
  mutate(rincome = fct_relevel(rincome, "Not applicable")) %>% 
  ggplot(aes(
    age,
    rincome
  ))+
  geom_point()


gss_cat %>% 
  filter(!is.na(age)) %>% 
  count(age, marital) %>% 
  group_by(age) %>% 
  mutate(prom = n / sum(n)) %>% 
  ggplot(aes(
    age,
    prom,
    colour = marital
  ))+
  geom_line(na.rm = TRUE)+
  theme(legend.position = "bottom")

gss_cat %>% 
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>% 
  ggplot(aes(
    marital,
    fill = marital
  ))+
  geom_bar()+
  theme(legend.position = "none")


gss_cat %>% 
  mutate(marital = marital %>% fct_infreq()) %>% 
  ggplot(aes(
    marital,
    fill = marital
  ))+
  geom_bar()+
  theme(legend.position = "none")


gss_cat %>% 
  count(tvhours) %>% 
  ggplot(aes(
    tvhours
  ))+
  geom_boxplot()+
  scale_y_log10()
  
  
gss_cat %>% 
  count(tvhours) %>% 
  ggplot(aes(
    tvhours,
    n
  ))+
  geom_col(width = 1)
  coord_flip()

  
  mean(gss_cat$tvhours, na.rm = TRUE)

  
  str(gss_cat)  
  
  # Modificaciones de los niveles de los factores 
  
  gss_cat %>% 
    count(partyid)
  
  
  gss_cat %>%
    mutate(partyid = fct_recode(partyid,
                                "No Responde" = "No answer",
                                "No sabe" = "Don't know",
                                "Otro Partido" = "Other party",
                                "Republicano Fuerte" = "Strong republican",
                                "Republicano Debil" = "Not str republican",
                                "Independente, cerca a Rep" = "Ind,near rep",
                                "Independente cerca a Dem" = "Ind,near dem",
                                "Democrata Debil" = "Not str democrat",
                                "Democrata Fuerte"= "Strong democrat",
                                "Independiente" = "Independent"
    )) %>%
    count(partyid)
  
  
  
  gss_cat %>%
    mutate(partyid = fct_recode(partyid,
                                "Otros" = "No answer",
                                "Otros" = "Don't know",
                                "Otros" = "Other party",
                                "Republicano Fuerte" = "Strong republican",
                                "Republicano Debil" = "Not str republican",
                                "Independente, cerca a Rep" = "Ind,near rep",
                                "Independente, cerca a Dem" = "Ind,near dem",
                                "Democrata Debil" = "Not str democrat",
                                "Democrata Fuerte"= "Strong democrat",
                                "Independiente" = "Independent"
    )) %>%
    count(partyid)  
  
  
  gss_cat %>%
    mutate(partyid = fct_collapse(partyid,
                                  Otros = c("No answer", "Don't know", "Other party"),
                                  Republicanos = c("Strong republican", "Not str republican"),
                                  Independientes = c("Ind,near rep", "Independent", "Ind,near dem"),
                                  Democratas = c("Not str democrat", "Strong democrat")
    )) %>%
    count(partyid)
  
  
  gss_cat %>% 
    mutate(relig = fct_lump(relig, 6)) %>% 
    count(relig, sort = TRUE)
  
  
  gss_cat %>% 
    mutate(relig = fct_lump(relig)) %>% 
    count(relig)

  gss_cat %>% 
    mutate(relig =  fct_lump(relig, 10)) %>% 
    count(relig, sort = TRUE)

  
  gss_cat %>% 
    mutate(relig =  fct_lump(relig, 10)) %>% 
    count(relig, sort = TRUE) %>% 
    mutate(prom = round(n/sum(n),3))
  