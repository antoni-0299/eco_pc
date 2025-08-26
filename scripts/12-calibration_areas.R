# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# limpieza de datos GBIF resumida

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(flexsdm)
library(sf)
library(tmap)
library(tidyverse)

# shapefiles --------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(sav|cos|crp|us.gpkg)',
  full.names = TRUE) %>%
  map(
    ~.x %>%
      read_sf() %>%
      st_make_valid()) %>%
  set_names(
    'costa_rica',
    'c_melanostictus',
    'crpn',
    'savage') %>%
  list2env(.GlobalEnv)

# data --------------------------------------------------------------------

c_melanostictus_sf <-
  read_rds('data/processed/occs_clean.rds') %>%
  filter(species == 'Craugastor melanostictus') %>%
  st_filter(costa_rica)

costa_rica %>%
  tm_shape() +
  tm_graticules(lines = FALSE) +
  tm_borders() +
tm_shape(c_melanostictus) +
  tm_borders() +
  tm_shape(c_melanostictus_sf) +
  tm_symbols(
    fill = 'source',
    size = 0.5)

tmap_mode('view')
tmap_mode('plot')

# segunda limpieza --------------------------------------------------------

# limpiar usando poligono de IUCN con buffer

c_melanostictus %>%
  st_buffer(5000) %>%
  st_intersection(costa_rica) %>%
  tm_shape() +
  tm_borders('red') +
  tm_shape(
    c_melanostictus %>%
      st_intersection(costa_rica)) +
  tm_borders('blue') +
  tm_shape(
    costa_rica,
    is.main = TRUE) +
  tm_borders() +
  tm_shape(c_melanostictus_sf) +
  tm_symbols(
    fill = 'source',
    size = 0.5)

c_melanostictus_cr_5km <-
  c_melanostictus %>%
  st_intersection(costa_rica) %>%
  st_buffer(5000) # esta es un area de calibracion

# crear datasets

c_melanostictus_occs_1 <-
  c_melanostictus_sf %>%
  st_filter(c_melanostictus_cr_5km) # adentro

c_melanostictus_occs_2 <-
  c_melanostictus_sf %>%
  st_filter(
    c_melanostictus_cr_5km,
    .predicate = st_disjoint) # afuera

# areas de calibracion ----------------------------------------------------

# otros tipos de area de calibracion

crs(savage, proj = TRUE)

# buffer ------------------------------------------------------------------

ca_1 <-
  calib_area(
    data = vect(c_melanostictus_occs_1),
    x = 'x',
    y = 'y',
    method = c(
      'buffer',
      width = 20000),
    crs = crs(savage))

plot(
  vect(costa_rica),
  main = 'Buffer method')

plot(ca_1, add = TRUE)

points(
  c_melanostictus_occs_1[, 2:3],
  pch = 19,
  cex = 0.5)

# sf

ca_1 <-
  calib_area(
    data = vect(c_melanostictus_occs_1),
    x = 'x',
    y = 'y',
    method = c(
      'buffer',
      width = 20000),
    crs = crs(savage)) %>%
  st_as_sf() %>%
  st_intersection(costa_rica)

ca_1 %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    costa_rica,
    is.main = TRUE) +
  tm_borders() +
  tm_shape(c_melanostictus_occs_1) +
  tm_symbols(
    fill = 'source',
    size = 0.5)

# mcp ---------------------------------------------------------------------

ca_2 <-
  calib_area(
    data = vect(c_melanostictus_occs_1),
    x = 'x',
    y = 'y',
    method = c('mcp'),
    crs = crs(savage)) %>%
  st_as_sf() %>%
  st_intersection(costa_rica)

ca_2 %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    costa_rica,
    is.main = TRUE) +
  tm_borders() +
  tm_shape(c_melanostictus_occs_1) +
  tm_symbols(
    fill = 'source',
    size = 0.5)

# mcp + buffer ------------------------------------------------------------

ca_3 <-
  calib_area(
    data = vect(c_melanostictus_occs_1),
    x = 'x',
    y = 'y',
    method = c(
      'bmcp',
      width = 5000),
    crs = crs(savage)) %>%
  st_as_sf() %>%
  st_intersection(costa_rica)

ca_3 %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    costa_rica,
    is.main = TRUE) +
  tm_borders() +
  tm_shape(c_melanostictus_occs_1) +
  tm_symbols(
    fill = 'source',
    size = 0.5)

# clusters ----------------------------------------------------------------

savage_v <-
  vect(savage)

ca_4 <-
  calib_area(
    data = vect(c_melanostictus_occs_1),
    x = 'x',
    y = 'y',
    method = c(
      'mask',
      savage_v,
      'clusters'),
    crs = crs(savage))

ca_4 %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    costa_rica,
    is.main = TRUE) +
  tm_borders() +
  tm_shape(c_melanostictus_occs_1) +
  tm_symbols(
    fill = 'source',
    size = 0.5)

savage_melanostictus <-
  savage %>%
  st_filter(c_melanostictus_occs_1)

c_melanostictus_occs_3 <-
  c_melanostictus_occs_2 %>%
  st_filter(savage_melanostictus)

c_melanostictus_occs <-
  bind_rows(
    c_melanostictus_occs_1,
    c_melanostictus_occs_3)

# mapa final --------------------------------------------------------------

ca_4 %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    costa_rica,
    is.main = TRUE) +
  tm_borders() +
  tm_shape(c_melanostictus_occs) +
  tm_symbols(
    fill = 'source',
    size = 0.5)
