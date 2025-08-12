# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Practica 4. Proyecciones con sf

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(sf)
library(tmap)
library(tidyverse)

# data --------------------------------------------------------------------

# crear capa vectorial a partir de coordenadas

species_coords <-
  read_csv('data/raw/species_coords.csv') %>%
  rename(
    x = longitude,
    y = latitude) %>%
  st_as_sf(
    coords = c('x', 'y'),
    crs = 4326,
    remove = FALSE)

st_crs(species_coords)

# shapefiles existentes

list.files(
  'shapefiles',
  pattern = ('eur|world'),
  full.names = TRUE) %>%
  map(~.x %>%
        read_sf() %>%
        st_make_valid())  %>%
  set_names(
    'rivers_europe',
    'world') %>%
  list2env(.GlobalEnv)

crs(
  world,
  proj = TRUE)

st_crs(world)

crs(
  rivers_europe,
  proj = TRUE)

st_crs(rivers_europe)

# mapeo rapido ------------------------------------------------------------

tm_shape(world) +
  tm_polygons(fill = 'continent') +
  tm_crs('auto') +
  tm_shape(species_coords) +
  tm_symbols(
    fill = 'species',
    size = 0.3,
    col = 'black',
    fill.legend = tm_legend('Species'),
    fill.scale =
      tm_scale_categorical(values = 'brewer.set2'))

tm_shape(rivers_europe) +
  tm_lines(col = 'blue') +
  tm_crs('auto')

# transformar objetos en vectores -----------------------------------------------

species_coords_vect <-
  vect(species_coords)

world_vect <-
  vect(world)

rivers_europe_vect <-
  vect(rivers_europe)

crs(
  species_coords_vect,
  proj = TRUE)

crs(
  world_vect,
  proj = TRUE)

crs(
  rivers_europe_vect,
  proj = TRUE)

plot(
  world_vect,
  border = 'grey') # borde

plot(
  species_coords_vect,
  'species',
  add = TRUE)

plot(
  rivers_europe_vect,
  col = 'blue') # lineas

# CRS ---------------------------------------------------------------------

tm_shape(world) +
  tm_polygons(fill = 'continent') +
  tm_crs(4326) +
  tm_shape(species_coords) +
  tm_symbols(
    fill = 'species',
    size = 0.3,
    col = 'black',
    fill.legend = tm_legend('Species'),
    fill.scale =
      tm_scale_categorical(values = 'brewer.set2')) +
  tm_shape(rivers_europe) +
  tm_lines(col = 'blue')

# funciona pero los crs son diferentes.

# cambiar proyeccion ------------------------------------------------------

world_laea <-
  st_transform(
    world,
    crs = st_crs(rivers_europe))

st_crs(world_laea)

species_coords_laea <-
  st_transform(
    species_coords,
    crs = st_crs(rivers_europe))

st_crs(species_coords_laea)

tm_shape(world_laea) +
  tm_polygons(fill = 'continent') +
  tm_shape(species_coords_laea) +
  tm_symbols(
    fill = 'species',
    size = 0.3,
    col = 'black',
    fill.legend = tm_legend('Species'),
    fill.scale =
      tm_scale_categorical(values = 'brewer.set2')) +
  tm_shape(rivers_europe) +
  tm_lines(col = 'blue')

# al reves ----------------------------------------------------------------

rivers_wgs84 <-
  st_transform(
    rivers_europe,
    crs = st_crs(world))

st_crs(rivers_wgs84)

tm_shape(world) +
  tm_polygons(fill = 'continent') +
  tm_crs('auto') +
  tm_shape(species_coords) +
  tm_symbols(
    fill = 'species',
    size = 0.3,
    col = 'black',
    fill.legend = tm_legend('Species'),
    fill.scale =
      tm_scale_categorical(values = 'brewer.set2')) +
  tm_shape(rivers_wgs84) +
  tm_lines(col = 'blue')

# proyectar raster --------------------------------------------------------

namibia_elevation <-
  rast('rasters/NAM_elv.tif')

# proyectar rasters

tm_shape(namibia_elevation) +
  tm_raster() +
  tm_shape(world) +
  tm_borders()

namibia_elevation_laea <-
  terra::project(
    namibia_elevation,
    world_laea,
    method = 'bilinear')

tm_shape(namibia_elevation_laea) +
  tm_raster() +
  tm_shape(world_laea) +
  tm_borders()

# mapa de europa sin Russia

world_eur <-
  world %>%
  filter(
    continent == 'Europe',
    name != 'Russia')


world_eur_laea <-
  world_eur %>%
  st_transform(
    crs = st_crs(world_laea))

tm_shape(world_eur) +
  tm_polygons(
    fill = 'burlywood',
    border = 'burlywood4') +
  tm_shape(world_eur_laea) +
  tm_borders(
    fill = 'burlywood4')

tm_shape(world_eur_laea) +
  tm_polygons(
    fill = 'burlywood') +
  tm_shape(world_eur_laea) +
  tm_borders(
    fill = 'burlywood4')

# Mollweide ---------------------------------------------------------------

world_moll <-
  world %>%
  st_transform(crs = 'ESRI:54009')

world_moll %>%
  tm_shape() +
  tm_polygons(fill = 'continent') +
  tm_crs('auto')
