# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# descarga de datos gbif. Una especie

# setup -------------------------------------------------------------------

rm(list = ls())

library(rgbif)
library(tidyverse)

# data --------------------------------------------------------------------

species <- 'Cephalopterus glabricollis'
my_species <- 'Cephalopterus_glabricollis'

key <-
  name_backbone(species) %>%
  pull(usageKey)

key

gbif_download <-
  occ_download(
    pred('taxonKey', key),
    format = 'SIMPLE_CSV')

gbif_download

# save citation -----------------------------------------------------------

# para salvar metadata

gbif_download %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_key.rds'))

# leer metadata

gbif_download <-
  read_rds(
  paste0(
  'data/raw/',
  my_species,
  '_key.rds'))

# check download processing -----------------------------------------------

# ver avance de la descarga en datasets grandes

occ_download_wait(gbif_download)

# crear objeto de descarga

data <-
  occ_download_get(
    gbif_download,
    path = 'data/raw',
    overwrite = TRUE) %>%
  occ_download_import()

# data <-
#   occ_download_get(
#     '0006515-250227182430271',
#     path = 'data/raw',
#     overwrite = TRUE)

# save data ---------------------------------------------------------------

# csv

data %>%
  write_csv(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.csv'))

read_csv(
  paste0(
    'data/raw/',
    my_species,
    '_gbif_raw.csv'))

# rds

data %>%
  write_rds(
    paste0(
      'data/raw/',
      my_species,
      '_gbif_raw.rds'))

read_rds(
  paste0(
    'data/raw/',
    my_species,
    '_gbif_raw.rds'))
