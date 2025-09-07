# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# UICN. Transformar shapefiles

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(tmap)
library(tidyverse)

# data --------------------------------------------------------------------

# species <-
#   read_sf('shapefiles/craugastor_melanostictus/data_0.shp')

# species %>%
#   tm_shape() +
#   tm_polygons() +
#   tm_shape(
#     species %>%
#       filter(
#         COMPILER == 'IUCN')) +
#   tm_polygons('red')
#
# # save shapefile ----------------------------------------------------------
#
# my_species <- 'craugastor_melanostictus'
#
# species %>%
#   write_sf(
#     paste0(
#     'shapefiles/',
#     my_species,
#     '.gpkg')) # usar nombre de especie
#
# rm(species)

# cargar ------------------------------------------------------------------

# c_fitzingeri <- read_sf('shapefiles/craugastor_fitzingeri.gpkg')
# c_melanostictus <- read_sf('shapefiles/craugastor_melanostictus.gpkg')

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
    'c_andi',
    'c_crassidigitus',
    'c_cuaquero',
    'c_fitzingeri',
    'c_melanostictus',
    'c_phasma',
    'c_rayo',
    'c_talamancae') %>%
  list2env(.GlobalEnv)

# chequear todos los poligonos

# https://www.iucnredlist.org/resources/spatial-data-legend

# c_fitzingeri # 2 poligonos. WGS84
# c_melanostictus # 1 poligono. WGS84

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
    'c_andi',
    'c_crassidigitus',
    'c_cuaquero',
    'c_fitzingeri',
    'c_melanostictus',
    'c_phasma',
    'c_rayo',
    'c_talamancae') %>%
  bind_rows()

group_polygon %>%
  select(species, presence, origin, legend) %>%
  st_drop_geometry()

tmap_mode('view')

group_polygon %>%
  filter(species == 'Craugastor cuaquero') %>%
  filter(compiler == 'IUCN') %>%
  tm_shape() +
  tm_polygons('red') +
  tm_shape(
    c_cuaquero %>%
      filter(compiler == 'Kelsey Neam')) +
  tm_polygons('red')

# calcular area -----------------------------------------------------------

group_area <-
  group_polygon %>%
  filter(presence != 6) %>%
  st_transform(crs = 3395) %>%
  mutate(
    area = st_area(geom) %>%
      units::set_units(km^2),
    .before = presence) %>%
  select(species, area) %>%
  st_drop_geometry()

group_area %>%
  mutate(area = area %>% as.numeric()) %>%
  summarise(area = sum(area))

fitzingeri_group_sf <-
  group_polygon %>%
  select(assessment:legend) %>%
  filter(presence != 6) %>%
  st_union() %>%
  st_sf()

# mapa --------------------------------------------------------------------

fitzingeri_group_map <-
  World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    group_polygon %>%
      filter(presence != 6),
    is.main = TRUE) +
  tm_polygons(fill = 'species')

tmap_mode('plot')

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    group_polygon,
    is.main = TRUE) +
  tm_polygons(fill = 'species') +
  tm_facets_grid('species')

fitzingeri_group_map <-
  World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    fitzingeri_group_sf,
    is.main = TRUE) +
  tm_polygons('darkgreen')

tmap_save(
  fitzingeri_group_map,
  'output/figures/fitzingeri_group_map.jpg',
  dpi = 300)
