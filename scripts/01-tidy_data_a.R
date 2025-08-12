# Ecologia de poblaciones
# Codigo 01b Lectura de datos ordenadas
# Profesor Hector Zumbado Ulate

# setup ----------------------------------------------------------

rm(list = ls())

library(tidyverse)

# data -------------------------------------------------------------

read_csv('data/raw/messy_weather.csv')

read_csv('data/raw/messy_weather.csv') %>%
  lobstr::ref()

# fixing data -------------------------------------------------

# Read in and assign the data:

messy_weather_long <-
  read_csv('data/raw/messy_weather.csv') %>%
  pivot_longer(
    cols = march_1:march_31,
    names_to = 'day',
    values_to = 'value',
    names_prefix = 'march_')

messy_weather_long

unique(messy_weather_long$checked)
unique(messy_weather_long$state)
unique(messy_weather_long$variable)
unique(messy_weather_long$Station)
unique(messy_weather_long$Name)

# fix names ---------------------------------------------------------------

messy_weather_long %>%
  select(
    station = Station,
    x = longitude,
    y,
    elevation,
    state,
    name = Name,
    year:value)

weather_names_fix <-
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
    year:value)


# fix variables -----------------------------------------------------------

weather_names_fix %>%
  pivot_wider(
    names_from = 'variable',
    values_from = 'value')

weather_variable_fix <-
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
    names_from = 'variable',
    values_from = 'value') %>%
  rename(precipitation = precip)

# create date -------------------------------------------------------------

weather_variable_fix %>%
  unite(
    col = 'date', #new column
    year:day,
    sep = '-') %>%
  mutate(date = as_date(date))

weather_date_fix <-
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
  mutate(date = as_date(date))

weather_date_fix

# fix temp ----------------------------------------------------------------

weather_date_fix %>%
  select(temperature_min_max)

weather_date_fix %>%
  select(temperature_min_max) %>%
  separate(
    temperature_min_max, #columna a separar
    into = c('temperature_min', 'temperature_max'),
    sep = ':')

weather_fix_temp <-
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
    sep = ':')

weather_fix_temp

# fix values --------------------------------------------------------------

weather_fix_temp %>%
  pivot_longer(
    cols = precipitation:temperature_max,
    names_to = 'variable',
    values_to = 'value') %>%
  filter(is.na(value))

weather_fix_temp %>%
  mutate(
    precipitation = as.numeric(precipitation),
    snow = as.numeric(snow),
    temperature_min = as.numeric(temperature_min),
    temperature_max = as.numeric(temperature_max))

# much better to use across

weather_fix_temp %>%
  mutate(
    across(
      precipitation:temperature_max,
      ~ as.numeric(.x)))

weather_value_temp <-
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

weather_value_temp

# grafico para compararar valores

weather_value_temp %>%
  pivot_longer(
    cols = precipitation:temperature_max,
    names_to = 'variable',
    values_to = 'value') %>%
  filter(!is.na(value)) %>%
  ggplot(
    (aes(x = variable, y = value))) +
  geom_boxplot() +
  theme_classic()

# individual tables -------------------------------------------------------

# there are two different dataframes in this dataset
# station data
# weather data

unique(weather_fix_temp$station)

weather_fix_temp %>%
  distinct(
    station,
    x,
    y,
    elevation,
    state,
    name)

weather_fix_temp %>%
  select(station:name) %>%
  distinct()

# Generate a station-level data frame and assign to the name `stations`:

stations <-
  weather_fix_temp %>%
  select(station:name) %>%
  distinct()

stations

# Generate a observation-level data frame and assign to the name
# `observations`:

observations <-
  weather_fix_temp %>%
  select(!x:name)

observations

stations %>%
  inner_join(observations)

# create a list to save ---------------------------------------------------

lst(stations, observations)

lst(
  stations =
    weather_fix_temp %>%
    select(station:name) %>%
    distinct(),

  observations =
    weather_fix_temp %>%
    select(!x:name)) %>%

  # Write to file:

  write_rds('data/processed/weather_tidy.rds')
