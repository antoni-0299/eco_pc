# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# UICN. Transformar shapefiles

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(tmap)
library(tidyverse)

# data --------------------------------------------------------------------

species <-
  read_sf('shapefiles/craugastor_crassidigitus/craugastor_crassidigitus.shp')

species %>%
  tm_shape() +
  tm_polygons() +
  tm_shape(
    species %>%
      filter(
        COMPILER == 'IUCN')) +
  tm_polygons('red')

# save shapefile ----------------------------------------------------------

my_species <- 'craugastor_crassidigitus'

species %>%
  write_sf(
    paste0(
    'shapefiles/',
    my_species,
    '.gpkg')) # usar nombre de especie

rm(species)

# cargar ------------------------------------------------------------------

 c_fitzingeri <- read_sf('shapefiles/craugastor_fitzingeri.gpkg')
 c_crassidigitus <- read_sf('shapefiles/craugastor_crassidigitus.gpkg')

list.files(
  'shapefiles',
  pattern = '^crau.*\\.gpkg$',
  full.names = TRUE) %>%
  map(~.x %>%
        read_sf() %>%
        st_make_valid() %>%
        janitor::clean_names() %>%
        rename(species = 'sci_name')) %>%
  set_names(
    'c_cuaquero',
    'c_fitzingeri',
    'c_crassidigitus',
    'c_melanosticus',
    'c_phasma',
    'c_talamancae') %>%
  list2env(.GlobalEnv)

# chequear todos los poligonos

# https://www.iucnredlist.org/resources/spatial-data-legend

c_fitzingeri # 2 poligonos. WGS84
c_melanostictus # 1 poligono. WGS84

view(c_fitzingeri)

c_fitzingeri %>%
  distinct(compiler)

c_fitzingeri %>%
  filter(compiler == 'IUCN') %>%
  tm_shape() +
  tm_borders()

c_fitzingeri %>%
  filter(compiler == 'Kelsey Neam') %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    c_fitzingeri %>%
      filter(compiler == 'IUCN')) +
  tm_polygons('red')

# calcular area -----------------------------------------------------------

c_fitzingeri_area <-
  c_fitzingeri %>%
  st_transform(crs = 3395) %>%
  # st_union() %>%
  st_area() %>%  # metros cuadrados
  units::set_units(km^2)

# leer grupo --------------------------------------------------------------

group_polygon <-
  list.files(
    'shapefiles',
    pattern = '^crau.*\\.gpkg$',
    full.names = TRUE) %>%
  map(~.x %>%
        read_sf() %>%
        st_make_valid() %>%
        janitor::clean_names() %>%
        rename(species = 'sci_name')) %>%
  set_names(
    'c_cuaquero',
    'c_fitzingeri',
    'c_crassidigitus',
    'c_melanosticus',
    'c_phasma',
    'c_talamancae') %>%
  bind_rows()

# calcular area -----------------------------------------------------------

group_area <-
  group_polygon %>%
  bind_rows() %>%
  st_transform(crs = 3395) %>%
  mutate(
    area = st_area(geom) %>%
      units::set_units(km^2),
    .before = presence) %>%
  select(species, area) %>%
  st_drop_geometry()


# mapa --------------------------------------------------------------------

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    group_polygon,
    is.main = TRUE) +
  tm_polygons(fill = 'species')

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    group_polygon,
    is.main = TRUE) +
  tm_polygons(fill = 'species') +
  tm_facets_grid('species')

tmap_save()


