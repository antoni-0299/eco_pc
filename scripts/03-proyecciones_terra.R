# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Practica 4. Proyecciones con terra

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
  vect(
    geom = c('x', 'y'),
    crs = 'epsg:4326',
    keepgeom = TRUE)

# shapefiles existentes

list.files(
  'shapefiles',
  pattern = ('eur|world'),
  full.names = TRUE) %>%
  map(~.x %>%
        vect())  %>%
  set_names(
    'rivers_europe',
    'world') %>%
  list2env(.GlobalEnv)

# check crs ---------------------------------------------------------------

crs(
  species_coords,
  proj = TRUE)

crs(
  world,
  proj = TRUE)

crs(
  rivers_europe,
  proj = TRUE)

# mapeo rapido ------------------------------------------------------------

plot(
  world,
  'continent') # poligonos

plot(
  world,
  border = 'grey') # borde

plot(
  species_coords,
  'species',
  add = TRUE)

plot(
  species_coords,
  pch = 20,
  cex = 1,
  'species')

plot(
  world,
  border = 'grey',
  add = TRUE) # borde

plot(
  rivers_europe,
  col = 'blue') # lineas

# transformar objetos en sf -----------------------------------------------

world_sf <-
  st_as_sf(world)

rivers_europe_sf <-
  st_as_sf(rivers_europe)

tm_shape(world) +
  tm_polygons(fill = 'continent')

tm_shape(rivers_europe_sf) +
  tm_lines(col = 'blue')

st_crs(world_sf)
st_crs(rivers_europe_sf)

# CRS ---------------------------------------------------------------------

plot(
  world,
  border = 'grey') # borde

plot(
  rivers_europe,
  col = 'royalblue',
  lwd = 2,
  add = TRUE)  # no se ven. Diferente CRS

plot(
  rivers_europe,
  col = 'royalblue')  # map looks OK

plot(
  world,
  add = TRUE)  # tampoco funciona

# cambiar proyeccion ------------------------------------------------------

world_laea <-
  project(world,
    rivers_europe)

plot(
  world_laea,
  border = 'grey')

plot(
  rivers_europe,
  col = 'blue',
  lwd = 2,
  add = TRUE)

species_coords_laea <-
  project(
    species_coords,
    rivers_europe)

plot(
  world_laea,
  border = 'grey')

plot(
  rivers_europe,
  col = 'blue',
  lwd = 2,
  add = TRUE)

plot(
  species_coords_laea,
  pch = 20,
  cex = 1,
  'species',
  add = TRUE)

# proyectar raster --------------------------------------------------------

namibia_elevation <-
  rast('rasters/NAM_elv.tif')

# proyectar rasters

plot(namibia_elevation)

namibia_elevation_laea <-
  terra::project(
    namibia_elevation,
    world_laea,
    method = 'bilinear')

plot(namibia_elevation_laea)

plot(
  world_laea,
  add = TRUE)

# mapa de europa sin Russia

world_eur <-
  subset(
    world,
    world$continent == 'Europe'
    & world$name != 'Russia')

world_eur_laea <-
  project(
    world_eur,
    world_laea)

plot(
  world_eur,
  col = 'burlywood',
  border = 'burlywood4')

plot(
  world_eur_laea,
  col = 'burlywood',
  border = 'burlywood4')

# al reves ----------------------------------------------------------------

rivers_wgs84 <-
  project(
    rivers_europe,
    world)

plot(
  world,
  border = 'grey')

plot(
  rivers_wgs84,
  col = 'blue',
  lwd = 2,
  add = TRUE)

plot(
  species_coords,
  pch = 20,
  cex = 1,
  'species',
  add = TRUE)

# Mollweide ---------------------------------------------------------------

world_moll <-
  project(
    world,
    '+proj=moll')

plot(world_moll)
