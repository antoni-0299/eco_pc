# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Practica 4. Mapeo terra

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(sf)
library(tmap)
library(tidyverse)

# data --------------------------------------------------------------------

species_coords <-
  read_csv("data/raw/species_coords.csv") %>%
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326,
    remove = FALSE)

list.files(
  'shapefiles',
  pattern = ('cit|lak|nam|riv'),
  full.names = TRUE) %>%
  map(~.x %>%
        read_sf() %>%
        st_make_valid()) %>%
  set_names(
    'cities',
    'rivers_europe',
    'lakes',
    'namibia_adm',
    'rivers') %>%
  list2env(.GlobalEnv)

# rasters -----------------------------------------------------------------

list.files(
  'rasters',
  pattern = '.tif',
  full.names = TRUE) %>%
  map(~.x %>%
        rast()) %>%
  set_names(
    'namibia_elevation',
    'population',
    'elevation',
    'bathymetry') %>%
  list2env(.GlobalEnv)

# colores ----------------------------------------------------------------

cols4all::c4a_gui()

# Africa ------------------------------------------------------------------

World %>%
  tm_shape() +
  tm_polygons(
    fill = 'continent',
    fill.scale =
      tm_scale_categorical(
        values = "matplotlib.seismic"),
    fill.legend =
      tm_legend(
        title = "Continent",
        orientation = "portrait",
        frame = TRUE))

World %>%
  tm_shape(crs = "+proj=robin") +
  tm_polygons(
    fill = "HPI",
    fill.scale =
      tm_scale_continuous(
        values = "matplotlib.rd_yl_bu"),
    fill.legend =
      tm_legend(
        title = "Happy Planet Index",
        orientation = "landscape",
        frame = FALSE))

# mapa Africa + especies

World %>%
  filter(continent == 'Africa') %>%
  tm_shape() +
  tm_polygons(fill = 'wheat') +
  tm_crs("auto") +
  tm_shape(species_coords) +
  tm_symbols(
    fill = 'species',
    fill.scale =
      tm_scale_categorical(
        values = "brewer.set2"),
    size = 0.5,
    col = 'black',
    fill.legend = tm_legend('Species'))

# mapa Namibia

namibia <-
  World %>%
  filter(name == 'Namibia')

# mapa africa + Namibia

World %>%
  filter(continent == 'Africa') %>%
  tm_shape() +
  tm_graticules(lines = FALSE) +
  tm_polygons(fill = 'wheat') +
  tm_crs("auto") +
  tm_shape(namibia) +
  tm_polygons('green')

# mapa africa + Namibia (mejorado)

World %>%
  tm_shape() +
  tm_graticules(lines = FALSE) +
  tm_polygons(fill = 'wheat') +
  tm_crs("auto") +
  tm_shape(
    World %>%
      filter(continent == 'Africa'),
    is.main = TRUE) +
  tm_polygons('wheat') +
  tm_shape(namibia) +
  tm_polygons('green')

# mapa namibia + especies

tm_shape(namibia) +
  tm_graticules(lines = FALSE) +
  tm_borders() +
  tm_shape(namibia_adm) +
  tm_borders() +
  tm_shape(
    species_coords %>%
      st_filter(namibia)) +
  tm_symbols(
    fill = 'species',
    fill.scale =
      tm_scale_categorical(
        values = "brewer.set2"),
    size = 0.6,
    col = 'black',
    fill.legend =
      tm_legend('Species'))

# cities ------------------------------------------------------------------

# plot with colours defined by column values:

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(cities) +
  tm_symbols(
    fill = 'CAPITAL',
    size = 0.4,
    col = 'black',
    fill.legend =
      tm_legend('Capital'),
    fill.scale =
      tm_scale_categorical(
        values = "brewer.set2"))

# countries ---------------------------------------------------------------

startswithB <-
  World %>%
  filter(
    str_detect(name, '^B'))

World %>%
  tm_shape() +
  tm_polygons('gray70') +
  tm_shape(startswithB) +
  tm_polygons('wheat')

# rivers------------------------------------------------------------------

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(rivers) +
  tm_lines(
    col = 'darkblue')

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(rivers) +
  tm_lines(
    col = 'SYSTEM')

# lakes -------------------------------------------------------------------

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(lakes) +
  tm_polygons(
    fill = 'lightblue')

World %>%
  # distinct(continent) %>%
  filter(continent == 'North America') %>%
  tm_shape() +
  tm_borders() +
  tm_shape(lakes) +
  tm_polygons(
    fill = 'lightblue')

# Namibia -----------------------------------------------------------------

tm_shape(namibia_adm) +
  tm_polygons(
    fill = 'wheat')

namibia_adm %>%
  distinct(NAME_2) %>%
  pull() %>%
  sort()

nam_subset <-
  namibia_adm %>%
  filter(
    NAME_2 %in% c(
      "Luderitz",
      "Gibeon",
      "Karas"))

tm_shape(namibia_adm) +
  tm_polygons(
    fill = 'wheat') +
  tm_shape(nam_subset) +
  tm_polygons(
    fill = 'darkgreen') +
  tm_shape(nam_subset) +
  tm_borders(
    lwd = 3) +
  tm_shape(nam_subset) +
  tm_borders(
    lwd = 1,
    col = 'white')

# all layers map ----------------------------------------------------------

World %>%
  tm_shape() +
  tm_polygons(
    fill = 'continent',
    fill.scale =
      tm_scale_categorical(
        values = "brewer.set2"),
    fill.legend =
      tm_legend(
        title = "Continent",
        orientation = "landscape",
        frame = FALSE)) +
  tm_shape(cities) +
  tm_symbols(
    fill = 'CAPITAL',
    size = 0.3,
    col = 'black',
    fill.legend =
      tm_legend('Capital'),
    fill.scale =
      tm_scale_categorical(
        values = c('darkgreen', 'orange'))) +
  tm_shape(lakes) +
  tm_polygons(
    fill = 'lightblue') +
  tm_shape(rivers) +
  tm_lines(
    col = 'darkblue')

# specific ranges ---------------------------------------------------------

extent <-
  st_bbox( #crear extent con paquete sf
    c(
      xmin = -86,
      xmax = -82.5,
      ymin = 7.5,
      ymax = 11.5),
    crs = 4326) %>%
  st_as_sfc() #transforma extent en poligono con paquetes

cr_map <-
  World %>%
  tm_shape(bb = extent) +
  tm_graticules(lines = F) +
  tm_polygons(fill = '#dadada')

World %>%
  tm_shape(bb = extent) +
  tm_borders()

extent <-
  st_bbox( #crear extent con paquete sf
    c(
      xmin = -20,
      xmax = 60,
      ymin = -40,
      ymax = 40),
    crs = 4326) %>%
  st_as_sfc() #transforma extent en poligono con paquetes

World %>%
  tm_shape(bb = extent) +
  tm_borders() +
  tm_shape(cities) +
  tm_symbols(
    fill = 'CAPITAL',
    size = 0.3,
    col = 'black',
    fill.legend =
      tm_legend('Capital'),
    fill.scale =
      tm_scale_categorical(
        values = c('darkgreen', 'orange'))) +
  tm_shape(lakes) +
  tm_polygons(
    fill = 'lightblue') +
  tm_shape(rivers) +
  tm_lines(
    col = 'darkblue')

# labels ------------------------------------------------------------------

World %>%
  tm_shape(bb = extent) +
  tm_text(
    'name',
    size = 0.75) +
  tm_borders()

# plots -------------------------------------------------------------------

bathymetry %>%
  tm_shape() +
  tm_raster()

elevation %>%
  tm_shape() +
  tm_raster(
    col.scale =
      tm_scale_continuous(
        values = terrain.colors(7)),
    col.legend =
      tm_legend(title = "Elevation (masl)"))

population %>%
  tm_shape() +
  tm_raster(
    col.scale =
      tm_scale(values = "geyser", midpoint = NA),
    tm_scale_intervals(style = "log10_pretty"),
    col.legend =
      tm_legend(title = "Elevation (masl)"))

# north arrow, scale, inset -----------------------------------------------

africa_map <-
  World %>%
  tm_shape(bb = extent) +
  tm_graticules(lines = FALSE) +
  tm_polygons('gray') +
  tm_scale_bar(
    breaks = c(0, 500, 1000)) +
  tm_compass(
    # position = c('bottom', 'left'),
    position = c(0.005, 0.98),
    type = "4star",
    size = 2) +
  tm_layout(
    bg.color = 'lightblue')

extent <-
  st_bbox(World) %>%
  st

bb <-
  sf::st_bbox(c(
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90),
    crs = 4326)

africa_map +
  tm_inset(
    bb,
    height = 5,
    width = 5,
    position = c("left", "bottom"))

# CRS ---------------------------------------------------------------------

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(rivers_europe) +
  tm_lines(col = 'blue')

st_crs(World)
st_crs(rivers_europe)

# CRS raster --------------------------------------------------------------

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(rivers_europe) +
  tm_lines('darkblue')

World %>%
  tm_shape() +
  tm_borders() +
  tm_shape(
    rivers_europe,
    is.main = TRUE) +
  tm_lines('darkblue')

rivers_europe %>%
  tm_shape() +
  tm_lines('darkblue') +
  tm_shape(World) +
  tm_borders()

# transformacion

world_laea <-
  st_transform(
    World,
    crs = st_crs(rivers_europe))

world_laea %>%
  tm_shape() +
  tm_borders() +
  tm_shape(rivers_europe) +
  tm_lines('darkblue')
