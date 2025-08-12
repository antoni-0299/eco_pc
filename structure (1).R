# setup -------------------------------------------------------------------

folders <-
  c('data/raw',
    'data/processed',
    'scripts',
    'shapefiles',
    'rasters',
    'output/figures',
    'output/tables',
    'docs',
    'other')

sapply(
  folders,
  FUN = dir.create,
  recursive = TRUE)
