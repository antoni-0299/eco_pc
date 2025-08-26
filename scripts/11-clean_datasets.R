# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# limpieza de datos GBIF resumida

# setup -------------------------------------------------------------------

rm(list = ls())

library(CoordinateCleaner)
library(scrubr)
library(sf)
library(tmap)
library(tidyverse)

# shapefiles --------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(cos|crp)',
  full.names = TRUE) %>%
  map(
    ~.x %>%
      read_sf() %>%
      st_make_valid()) %>%
  set_names(
    'costa_rica', 'crpn') %>%
  list2env(.GlobalEnv)

# data --------------------------------------------------------------------

# max_uncertainty <- (5000 * sqrt(2)) / 2

# inaturalist

inat_occurrences <-
  list.files(
  'data/raw/',
  pattern = 'inat.*\\.csv',
  full.names = TRUE) %>%
  map(
    ~ .x %>%
      read_csv() %>%
      filter(
        coordinates_obscured != 'false') %>%
      select(
        id,
        species = scientific_name,
        date = observed_on,
        x = longitude,
        y = latitude,
        accuracy = positional_accuracy) %>%
      mutate(
        date = ymd(date),
        source = 'iNaturalist',
        id = paste(
          'inat_',
          id,
          sep = '')) %>%
        filter(
          !if_any(
            c(date, x:y),
            ~is.na(.x))) %>%
        filter(year(date) >= 1950) %>%
        # filter(accuracy < max_uncertainty) %>%
        select(
          species,
          x:y,
          everything()) %>%
      distinct(
        x,
        y,
        date,
        .keep_all = TRUE) %>%
      coord_incomplete() %>%
      coord_imprecise() %>%
      coord_impossible() %>%
      coord_unlikely() %>%
      clean_coordinates(
        lon = 'x',
          lat = 'y',
          tests = c(
            'capitals',
            'centroids',
            'equal',
            'gbif',
            'institutions',
            'seas',
            'zeros'),
          species = 'species',
          value = 'clean') %>%
        st_as_sf(
          coords = c('x', 'y'),
          crs = 4326,
          remove = FALSE)) %>%
  bind_rows()

# check species and year

inat_occurrences %>%
  distinct(species)

inat_occurrences <-
  inat_occurrences %>%
  filter(
    species %in% c(
      'Craugastor fitzingeri',
      'Craugastor melanostictus')) %>%
  arrange(year(date))

# gbif

gbif_occurrences <-
  read_csv(
      'data/raw/craugastor_fitzingeri_group_gbif_raw.csv') %>%
  group_by(species) %>%
  filter(institutionCode != 'iNaturalist') %>%
  select(
    id = gbifID,
    species,
    month,
    day,
    year,
    date = eventDate,
    x = decimalLongitude,
    y = decimalLatitude,
    accuracy = coordinateUncertaintyInMeters) %>%
  filter(!is.na(year)) %>%
  mutate(
    across(month:day,
           ~replace_na(.x, 1))) %>%
  unite(
    'date',
    c(year, month, day),
    sep = '-') %>%
  mutate(
    date = ymd(date),
    source = 'GBIF',
    id = paste(
      'gbif',
      id,
      sep = '_')) %>%
  # filter(accuracy < max_uncertainty) %>%
  filter(year(date) >= 1950) %>%
  filter(
    !if_any(
      c(date, x:y),
      ~is.na(.x))) %>%
  select(
    species,
    x:y,
    everything()) %>%
  distinct(
    x,
    y,
    date,
    id,
    .keep_all = TRUE) %>%
  rename(
    longitude = x,
    latitude = y) %>%
  coord_incomplete() %>%
  coord_imprecise() %>%
  coord_impossible() %>%
  coord_unlikely() %>%
  clean_coordinates(
    lon = 'longitude',
    lat = 'latitude',
    tests = c(
      'capitals',
      'centroids',
      'equal',
      'gbif',
      'institutions',
      'seas',
      'zeros'),
    species = 'species',
    value = 'clean') %>%
  rename(
    x = longitude,
    y = latitude) %>%
  st_as_sf(
    coords = c('x', 'y'),
    crs = 4326,
    remove = FALSE) %>%
  ungroup()

gbif_occurrences %>%
  distinct(species)

sort(names(gbif_occurrences))
sort(names(inat_occurrences))

occurrences <-
  bind_rows(
    inat_occurrences,
    gbif_occurrences)

# map ---------------------------------------------------------------------

World %>%
  tm_shape() +
  tm_graticules(lines = F) +
  tm_polygons('gray70') +
  tm_crs('auto') +
  tm_shape(
    occurrences,
    is.main = TRUE) +
  tm_dots(
    fill = 'source',
    size = 0.5,
    shape = 21,
    fill.legend =
      tm_legend(
        title = 'Source')) +
  tm_scalebar(
    breaks = c(0, 250, 500),
    position = c('bottom', 'left')) +
  tm_compass(
    position = c('top', 'right')) +
  tm_layout(
    bg.color = 'lightblue')

extent <-
  st_bbox( #crear extent con paquete sf
    c(
      xmin = -86,
      xmax = -82,
      ymin = 8,
      ymax = 11.3,
      crs = 4326)) %>%
  st_as_sfc() #transforma extent en poligono

crpn %>%
  tm_shape(
    bb = extent) +
  tm_polygons('gray70') +
  tm_crs(4326) +
  tm_graticules(lines = F) +
  tm_shape(occurrences) +
  tm_dots(
    fill = 'source',
    size = 0.5,
    shape = 21,
    fill.legend =
      tm_legend(
        title = 'Source')) +
  tm_scalebar(
    breaks = c(0, 50, 100),
    position = c('bottom', 'left')) +
  tm_compass(
    position = c('top', 'right')) +
  tm_layout(
    bg.color = 'lightblue')

crpn %>%
  tm_shape(bb = extent) +
  tm_polygons('gray70') +
  tm_crs(4326) +
  tm_graticules(lines = F) +
  tm_shape(
    occurrences %>%
      st_filter(costa_rica)) +
  tm_dots(
    fill = 'source',
    size = 0.5,
    shape = 21,
    fill.legend =
      tm_legend(
        title = 'Source')) +
  tm_scalebar(
    breaks = c(0, 50, 100),
    position = c('bottom', 'left')) +
  tm_compass(
    position = c('top', 'right')) +
  tm_layout(
    bg.color = 'lightblue')

# save clean dataset ------------------------------------------------------

occurrences %>%
  write_rds(
    paste0(
      'data/processed/occs_clean.rds'))
