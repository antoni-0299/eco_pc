# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Practica 4. Mapeo terra

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(sf)
library(car)
library(tidyverse)

# data --------------------------------------------------------------------

species_coords <-
  read_csv('data/raw/species_coords.csv') %>%
  rename(
    x = longitude,
    y = latitude) %>%
  vect(
    geom = c('x', 'y'),
    crs = 'epsg:4326',
    keepgeom = TRUE)

list.files(
  'shapefiles',
  pattern = ('cit|lak|nam|riv|world'),
  full.names = TRUE) %>%
  map(~.x %>%
        vect()) %>%
  set_names(
    'cities',
    'rivers_europe',
    'lakes',
    'namibia_adm',
    'rivers',
    'world') %>%
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

# occurrences -------------------------------------------------------------

plot(species_coords, 'species')
plot(species_coords, 'country')

plot(
  species_coords,
  'species',
  col = hcl.colors(
    length(
      unique(species_coords$species))))

plot(
  species_coords,
  'country',
  col = rainbow(
    length(
      unique(species_coords$country))))

# world ------------------------------------------------------------------

plot(
  world,
  'continent',
  border = 'gray')

plot(world,
     'region',
     main = 'Map of the world',
     col = hcl.colors(6, 'geyser'))

world$startswithB <-
  startsWith(as.character(world$name), 'B')

plot(
  world,
  'startswithB',
  lwd = 0.1)

# cities ------------------------------------------------------------------

cities

plot(cities)

cities %>%
  values() %>%
  as_tibble()

# plot with colours defined by column values:

plot(
  cities,
  'CAPITAL')

plot(
  cities,
  'CAPITAL',
  col = c('orange', 'darkgreen'),
  plg = list(x = 'topright'))

# customize legend size:

plot(
  cities,
  'COUNTRY',
  mar = c(2, 2, 2, 15),
  plg = list(ncol = 4))

# rivers------------------------------------------------------------------

rivers %>%
  values() %>%
  as_tibble()

plot(
  rivers,
  col = 'blue')

plot(
  rivers,
  'SYSTEM')

perim(rivers)
st_length(st_as_sf(rivers))

# lakes -------------------------------------------------------------------

lakes %>%
  values() %>%
  as_tibble()

plot(
  lakes,
  col = 'blue',
  border = NA)

# Namibia -----------------------------------------------------------------

sort(unique(namibia_adm$NAME_2))

nam_subset <-
  subset(
    namibia_adm,
    namibia_adm$NAME_2 %in% c(
      'Luderitz',
      'Gibeon',
      'Karas'))

plot(
  namibia_adm,
  'NAME_1',
  col = 'wheat',
  main = 'Study area')

plot(
  nam_subset,
  col = 'darkgreen',
  lwd = 7,
  add = TRUE)

plot(
  nam_subset,
  border = 'white',
  add = TRUE)

text(
  nam_subset,
  'NAME_2',
  cex = 0.8,
  halo = TRUE)

# all layers map ----------------------------------------------------------

sort(unique(world$region))

plot(
  world,
  'region',
  main = 'Map of the world',
  col = topo.colors(22))

plot(
  world,
  'continent',
  main = 'Map of the world',
  col = terrain.colors(6))

plot(
  cities,
  cex = 0.4,
  col = 'darkred',
  add = TRUE)

plot(
  rivers,
  col = 'blue',
  add = TRUE)

plot(
  lakes,
  col = 'lightblue',
  border = NA,
  add = TRUE)

plot(
  species_coords,
  col = 'orange',
  pch = 4,
  cex = 0.2,
  add = TRUE)

# specific ranges ---------------------------------------------------------

plot(
  world,
  border = 'grey')

my_window <-
  ext(-20, 60, -40, 40)  # xmin, xmax, ymin, ymax

plot(
  world,
  border = 'grey',
  ext = my_window)

plot(
  rivers,
  col = 'blue',
  add = TRUE)

plot(
  cities,
  cex = 0.3,
  col = 'darkred',
  add = TRUE)

plot(
  species_coords,
  col = 'darkgreen',
  pch = 19,
  cex = 0.5,
  add = TRUE)

# labels ------------------------------------------------------------------

plot(
  world,
  border = 'darkgrey',
  ext = my_window)

text(
  x = world,
  'name',
  cex = 0.7)

world_centroids <-
  world %>%
  centroids() %>%
  crds()

plot(
  world,
  border = 'darkgrey',
  ext = my_window)

set.seed(1)

label_coords <-
  pointLabel(
  x = world_centroids,
  labels = world$name,
  cex = 0.7)

label_coords$y[which(world$name == 'South Africa')]

label_coords$y[which(world$name == 'South Africa')] <- -32

#map again

plot(
  world,
  border = 'darkgrey',
  ext = my_window)

text(
  label_coords$x,
  label_coords$y,
  world$name,
  col = 'blue',
  cex = 0.7)

# check layers ------------------------------------------------------------

# notice different dimensions, resolution, extent

elevation

population

world

ext(elevation)  # spatial extent (min and max coordinates)
crs(elevation, proj = TRUE)  # crs in 'proj' format
res(elevation)  # spatial resolution (pixel size)

ext(world)
crs(world, proj = TRUE)
head(world)

# colours -----------------------------------------------------------------

colours()  # you can search 'R colours' online for images

plot(
  world,
  ext = my_window,
  col = 'wheat',
  border = 'wheat4',
  background = 'lightblue')

plot(
  rivers,
  col = 'royalblue',
  lwd = 2,
  add = TRUE)

plot(
  cities,
  pch = 20,
  col = 'darkred',
  cex = 0.7,
  add = TRUE)

text(
  subset(
    cities,
    cities$CAPITAL == 'Y'),
  'NAME',
  cex = 0.7,
  col = 'darkgreen',
  halo = TRUE)

# palette rasters ---------------------------------------------------------

plot(bathymetry)
plot(elevation)
plot(population)

plot(
  elevation,
  col = cm.colors(
    100,
    rev = T))

plot(
  population,
  add = TRUE,
  alpha = 0.6,
  legend = FALSE)

plot(
  elevation,
  col = hcl.colors(100))

plot(
  elevation,
  col = hcl.colors(
    100,
    palette = 'inferno'))

plot(
  elevation,
  col = hcl.colors(
    100,
    palette = 'spectral'))

plot(
  elevation,
  col = hcl.colors(
    100,
    palette = 'geyser'))

plot(
  elevation,
  col = rainbow(100))

plot(
  elevation,
  col = terrain.colors(100))

plot(elevation, col = topo.colors(100))

plot(
  elevation,
  col = cm.colors(100))

plot(
  elevation,
  col = heat.colors(100))

plot(
  elevation,
  col = topo.colors(100, rev = TRUE))

plot(
  elevation,
  col = hcl.colors(
    100,
    palette = 'spectral',
    rev = TRUE))

# legend breaks -----------------------------------------------------------

elevation

pretty_breaks <-
  pretty(
    elevation$wc2.1_5m_elev,
    n = 10)

plot(
  elevation,
  'wc2.1_5m_elev',
  col = hcl.colors(
    100,
    palette = 'geyser'),
  breaks = pretty_breaks)

plot(population)

plot(
  population,
  breaks = quantile(
    values(population),
    na.rm = TRUE))

# north arrow, scale, inset -----------------------------------------------

plot(
  world,
  border = 'grey',
  ext = my_window)

plot(
  species_coords,
  add = TRUE)

north(
  col = 'darkblue',
  font = 2,
  lwd = 5)

sbar(1000)  # default

sbar(
  xy = c(-15, -25),
  d = 1000,
  type = 'bar',
  below = 'km')

plot(
  world,
  border = 'grey',
  ext = my_window)

inset(
  world,
  loc = 'topleft',
  border = 'grey',
  box = ext(my_window),
  pbox = list(col = 'darkred', lwd = 2))

# bubble maps -------------------------------------------------------------

cities$is_capital <-
  ifelse(
    cities$CAPITAL == 'Y',
    1,
    0)

plot(
  world,
  border = 'grey')

plot(
  cities,
  pch = 1,
  col = 'darkblue',
  cex = cities$is_capital + 0.5,
  add = TRUE)

# pdf map -----------------------------------------------------------------

pdf(
  'output/figures/world_map.pdf',
  width = 7,
  height = 4)

dev.off()

# CRS raster --------------------------------------------------------------

namibia_elevation

crs(
  namibia_elevation,
  proj = TRUE)  # longlat WGS84

namibia_elevation[]  # show the values of the raster pixels

values(namibia_elevation)  # same

plot(namibia_elevation)

# check available country names and ISO codes:
 names(geodata::country_codes())

 geodata::country_codes()[ , c('NAME', 'ISO3')]

 geodata::country_codes() %>%
   as_tibble

# ggplot ------------------------------------------------------------------

crs(species_coords) <-
   crs(world)

world  %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    aes(fill = continent),
    lwd = 0.2) +
  geom_sf(
    data = st_as_sf(species_coords),
    colour = 'darkgreen',
    size = 0.5) +
  geom_sf(
    data = st_as_sf(rivers),
    color = 'blue',
    lwd = 0.6) +
  geom_sf(
    data =
      st_as_sf(cities),
    colour = 'darkred',
    size = 0.5,
    shape = 23) +
  coord_sf(crs = '+proj=moll') +
  theme_classic()
