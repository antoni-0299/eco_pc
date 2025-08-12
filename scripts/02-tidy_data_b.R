# Ecologia de poblaciones
# Codigo 01b Lectura de datos ordenadas
# Profesor Hector Zumbado Ulate

# setup ----------------------------------------------------------

rm(list = ls())

library(tidyverse)

# data -------------------------------------------------------------

weather_data <-
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer(
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_') %>%
  select(
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value) %>%
  pivot_wider(
    names_from = variable) %>%
  rename(precipitation = precip) %>%
  unite(
    col = 'date',
    year:day,
    sep = '-') %>%
  mutate(date = as_date(date)) %>%
  separate(
    temperature_min_max,
    into = c('temperature_min', 'temperature_max'),
    sep = ':') %>%
  mutate(
    across(
      precipitation:temperature_max,
      ~ as.numeric(.x)))

# create a list to save ---------------------------------------------------

list(
  stations =
    weather_data %>%
    select(station:name) %>%
    distinct(),
  observations =
    weather_data %>%
    select(!x:name)) %>%
  write_rds('data/processed/weather_tidy.rds')
