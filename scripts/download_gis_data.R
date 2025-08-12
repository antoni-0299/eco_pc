# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Practica 4. Descargar gis data

# setup -------------------------------------------------------------------

rm(list = ls())

library(geodata)
library(rnaturalearth)

# data --------------------------------------------------------------------

elevation <-
  elevation_global(
    res = 5,
    path = "rasters")

population <-
  population(
    year = 2020,
    res = 2.5,
    path = "rasters")

lakes <-
  ne_download(
    scale = 110,
    type = "lakes",
    category = "physical",
    destdir = "output/shapefiles",
    returnclass = 'sf')

# lakes %>%
#   write_sf('shapefiles/lakes.gpkg')

namibia_adm <-
  gadm(
    country = "Namibia",
    level = 2,
    path = "output/shapefiles",
    returnclass = 'sf')

namibia_adm %>%
  st_as_sf() %>%
  write_sf('shapefiles/namibia_adm.gpkg')

namibia_adm %>%
  writeVector(
    'output/namibia_adm.gpkg',
    overwrite = TRUE)

elev_namibia <-
  elevation_30s(
    country = 'Namibia',
    path = 'rasters',
    mask = FALSE)

# check available country names and ISO codes. lib geodata
names(country_codes())

country_codes()[ , c("NAME", "ISO3")]

country_codes() %>%
  as_tibble()
