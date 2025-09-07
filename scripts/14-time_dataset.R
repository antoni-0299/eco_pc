# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Distribucion temporal

# Setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(tmap)
library(tidyverse)

# shapefiles --------------------------------------------------------------

crpn <-
  read_sf('shapefiles/crpanic.gpkg') %>%
  st_make_valid() %>%
  janitor::clean_names()

# data --------------------------------------------------------------------

my_species <- "Craugastor_fitzingeri"

my_species_cr <-
  read_rds('data/processed/occs_clean.rds') %>%
  filter(species == 'Craugastor fitzingeri') %>%
  st_filter(crpn)

# pre-processing ----------------------------------------------------------

my_species_sf <-
  my_species_cr %>%
  st_as_sf(
    coords = c('x', 'y'),
    crs = 4326,
    remove = FALSE)

# pre_map -----------------------------------------------------------------

extent <-
  st_bbox( #crear extent con paquete sf
    c(
      xmin = -86.5,
      xmax = -82.5,
      ymin = 7.5,
      ymax = 11.5),
    crs = 4326) %>%
  st_as_sfc() #transforma extent en poligono con paquetes

# Dataset antes 2000 --------------------------------------------------------

my_species_cr_pre <-
  my_species_sf %>%
  filter(year(date) < 2000)

# my_species_cr_pre %>%
#   write_rds(
#     paste0(
#       'data/processed/',
#       my_species,
#       '_predecline.rds'))

# Dataset despues 2000 --------------------------------------------------------

my_species_cr_post <-
  my_species_sf %>%
  filter(year(date) >= 2000)

# my_species_cr_post %>%
#   write_rds(
#     paste0(
#       'data/processed/',
#       my_species,
#       '_postdecline.rds'))

# Map ---------------------------------------------------------------------

temp_dist <-
  crpn %>%
  tm_shape(bb = extent) +
  tm_graticules(
    lines = F,
    labels.size = 0.8) +
  tm_polygons(col = 'cornsilk3') +
  tm_shape(costa_rica) +
  tm_borders(
    col = 'cornsilk3',
    lwd = 2) +
  tm_shape(
    my_species_cr_pre) +
  tm_dots(
    fill = 'blue',
    size = 0.3) +
  tm_shape(
    my_species_cr_post) +
  tm_symbols(
    fill = 'tan1',
    size = 0.3) +
  tm_add_legend(
    title = "Observation date",
    "symbol",
    labels = "Before 2000",
    fill = 'cornflowerblue') +
  tm_add_legend(
    "symbol",
    labels = "2000 or after",
    fill = 'salmon') +
  tm_compass(
    position = c('left', 'top'),
    text.size = 0.3,
    type = '4star') +
  tm_scalebar(
    position = c('right', 'bottom'),
    text.size = 0.8,
    breaks = c(0, 50, 100)) +
  tm_layout(
    legend.position = c(0.03, 0.3),
    bg.color = 'lightblue1',
    legend.bg.color = 'white',
    legend.text.size = 1,
    legend.title.size = 1.2,
    legend.title.fontface = 'bold')

temp_dist

# save map ----------------------------------------------------------------

tmap_save(
  temp_dist,
  paste0(
    'output/figures/',
    my_species,
    '_temporal_distribution.jpg'),
  height = 7,
  width = 8,
  dpi = 300)
