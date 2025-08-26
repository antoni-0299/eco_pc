# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# descarga de datos gbif. Multiples especies

# setup -------------------------------------------------------------------

rm(list = ls())

library(rgbif)
library(tidyverse)

# lista de especies -------------------------------------------------------

species_list <-
  "https://raw.githubusercontent.com/hzumbado/eco_pc/refs/heads/main/data/raw/craugastor_fitzingeri_group.csv"

# species_list %>%
#   write_csv('data/raw/craugastor_fitzingeri_group.csv')

long_checklist <-
  read_csv(species_list)

# match the names

gbif_taxon_keys <-
  long_checklist %>%
  name_backbone_checklist() %>% # match to backbone
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey)

# gbif_taxon_keys should be a long vector like this (2430837 2430627 2430869 2430621 2430658 2430703 2430699 2430811)

# download the data

gbif_download <-
  occ_download(
  pred_in("taxonKey", gbif_taxon_keys), # important to use pred_in
  pred("hasCoordinate", TRUE),
  format = "SIMPLE_CSV")

gbif_download

# save citation -----------------------------------------------------------

# para salvar metadata

gbif_download %>%
  write_rds('data/raw/craugastor_fitzingeri_group_key.rds')

# leer metadata

gbif_download <-
  read_rds(
    'data/raw/craugastor_fitzingeri_group_key.rds')

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
      'data/raw/craugastor_fitzingeri_group_gbif_raw.csv'))

read_csv(
  'data/raw/craugastor_fitzingeri_group_gbif_raw.csv')

# rds

data %>%
  write_rds(
    'data/raw/craugastor_fitzingeri_group_gbif_raw.rds')

read_rds(
  'data/raw/craugastor_fitzingeri_group_gbif_raw.rds')
