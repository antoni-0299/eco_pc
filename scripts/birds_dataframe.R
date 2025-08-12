#Ecología de poblaciones


#Setup

rm(list=ls())

library(tidyverse)

#data

read.csv("data/raw/pajaritos.csv", sep = ";") %>%
  view()

pajaritos_raw <-
  read.csv("data/raw/pajaritos.csv", sep = ";")

names(pajaritos_raw)


pajaritos_df<- pajaritos_raw %>%
  select(
    site = "Punto.de.muestreo",
    x = X,
    y = Y,
    species = Sp,
    sex = Sexo,
    age = "Estado.de.desarrollo",
    substrate = Estrato,
    human_presence = "Presencia.humana")

pajaritos_df %>%
  mutate(campus = "omar_dengo",
         .before = site) %>%
  mutate(
    year = 2025,
    month = 08,
    day = 05) %>%
  unite(
    col = "date",
    year:day,
    sep = "-") %>%
  mutate(
    date = as_date(date))%>%
      mutate(
        across(
          c(campus, site, sex:substrate),
          ~.x %>% as_factor()))

