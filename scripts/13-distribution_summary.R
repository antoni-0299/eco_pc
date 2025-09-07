# Ecologia de poblaciones
# Profesor Hector Zumbado Ulate
# Distribucion geografica

# Setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(tmap)
library(tidyverse)

# Shapefiles --------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(co|sav|can)',
  full.names = TRUE) %>%
  map(
    ~ .x %>%
      read_sf() %>%
      st_make_valid() %>%
      janitor::clean_names()) %>%
  set_names(
    'costa_rica',
    'cantones',
    'Savage') %>%
  list2env(.GlobalEnv)

# data --------------------------------------------------------------------

my_species <- "Craugastor_fitzingeri"

my_species_cr <-
  read_rds('data/processed/occs_clean.rds') %>%
  filter(species == 'Craugastor fitzingeri') %>%
  st_filter(costa_rica)

# pre-processing ----------------------------------------------------------

my_species_sf <-
  my_species_cr %>%
    st_as_sf(
    coords = c('x', 'y'),
    crs = 4326,
    remove = FALSE) %>%
  st_join(cantones) %>%
  select(
    species:accuracy,
    canton,
    cod,
    source,
    everything())

# Distribucion cantonal --------------------------------------------------

cantones_my_species <-
  cantones %>%
  st_filter(my_species_sf)

cantones_count <-
  my_species_sf %>%
  as_tibble() %>%
  summarize(
    n = n(),
    .by = canton) %>%
  full_join(
    cantones,
    .,
    by = 'canton')

cantones_count

cantones_map <-
  costa_rica %>%
  tm_shape() +
  tm_borders() +
  tm_shape(cantones_count) +
  tm_polygons(
    fill = "n",
    fill.scale =
      tm_scale_continuous(
        values = "-orange_blue_diverging",
        label.na = 'Not recorded'),
    fill.legend =
      tm_legend(
        "Observations",
        group_id = "top"),
    col = NULL) +
  tm_shape(my_species_sf) +
  tm_symbols(
    size = 0.4,
    fill = 'red') +
  tm_shape(cantones) +
  tm_borders(lwd = 1) +
  tm_compass(group_id = "bottom") +
  tm_scalebar(
    group_id = "bottom",
    breaks = c(0, 10, 50)) +
  tm_comp_group(
    "bottom",
    position = tm_pos_in(
      "left",
      "bottom",
      align.h = "left"))

cantones_map

tmap_mode('view')

cantones_map

tmap_mode('plot')

# mapa en ggplot

extent <-
  st_bbox( #crear extent con paquete sf
    c(
      xmin = -86,
      xmax = -82.5,
      ymin = 7.5,
      ymax = 11.5),
    crs = 4326)

cantones_count %>%
  ggplot() +
  geom_sf(
    aes(fill = n)) +
  scale_fill_viridis_c(
    option = 'plasma',
    na.value = '#dcdcdc') +
  coord_sf(
    xlim = c(extent$xmin, extent$xmax),
    ylim = c(extent$ymin, extent$ymax)) +
  theme_classic() #mismo mapa en ggplot.

# Distribucion historica --------------------------------------------------

county_history <-
  my_species_sf %>%
  group_by(canton) %>%
  summarize(
    n = n()) %>%
  ggplot(
    aes(x = reorder(canton, n),
        y = n)) +
  geom_bar(
    stat = 'identity',
    fill = 'cornflowerblue',
    col = 'black') +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(0, 20, 40, 60, 80)) +
  labs(
    x = 'County',
    y = 'Observations') +
  theme_classic()

county_history

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_cantones.jpg'),
  dpi = 300)

# Distribucion temporal --------------------------------------------------

my_species_sf %>%
  as_tibble() %>%
  group_by(canton) %>%
  summarize(n = n()) %>%
  arrange(
    desc(n)) %>%
  print(n = 40) # slice (1:10)

dist_temp <-
  my_species_sf %>%
  group_by(year = year(date)) %>%
  summarize(n = n()) %>%
  ggplot(
    aes(year, n)) +
  geom_bar(
    stat = 'identity',
    col = 'black',
    fill = 'cornflowerblue') +
  scale_y_continuous(
    limits = c(0, 300),
    expand = c(0,0)) +
  labs(
    x = 'Year',
    y = 'Observation since 1950') +
  theme_classic()

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_year.jpg'),
  dpi = 300)


# Distribucion desde el 2015 ----------------------------------------------

dist_temp2 <-
  my_species_sf %>%
  filter(year(date) %in% 2015:2025) %>%
  group_by(canton) %>%
  summarize(n = n()) %>%
  ggplot(
    aes(x = reorder(canton, n),
        y = n)) +
  geom_bar(
    stat = 'identity',
    fill = 'salmon') +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = paste('Observaciones de',  my_species, 'por canton desde 2015'),
    x = 'Canton',
    y = 'Numero de observaciones') +
  theme_classic()

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_year.jpg'),
  dpi = 300)

# Distribucion latitudinal historica --------------------------------------------------

lat <-
  my_species_sf %>%
  group_by(year = year(date)) %>%
  summarize(
    sur = min(y),
    norte = max(y)) %>%
  pivot_longer(
    sur:norte,
    names_to = 'limites',
    values_to = 'latitud') %>%
  ggplot(
    aes(x = year,
        y = latitud)) +
  geom_point() +
  geom_line(aes(col = limites)) +
  scale_color_manual(
    values = c('blue', 'orange'),
    name = 'Limits',
    labels = c("North", "South")) +
  labs(
    x = 'Year',
    y = 'Latitude') +
  theme_classic() +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.position.inside = c(.5, .3),
    legend.background = element_rect(
      fill = "gray90",
      linewidth =  0.5,
      linetype = "solid",
      colour = "gray20"))

lat

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_latitude.jpg'),
  dpi = 300)

# Distribucion longitudinal historica --------------------------------------------------

long <-
  my_species_sf %>%
  group_by(year = year(date)) %>%
  summarize(
    oeste = min(x),
    este = max(x)) %>%
  pivot_longer(
    oeste:este,
    names_to = 'limites',
    values_to = 'longitud') %>%
  ggplot(
    aes(
      x = year,
      y = longitud)) +
  geom_point() +
  geom_line(aes(col = limites)) +
  scale_color_manual(
    values = c('blue', 'orange'),
    name = 'Limits',
    labels = c("West", "East")) +
  labs(
    x = 'Year',
    y = 'Latitude') +
  theme_classic() +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.position.inside = c(.5, .3),
    legend.background = element_rect(
      fill = "gray90",
      linewidth =  0.5,
      linetype = "solid",
      colour = "gray20"))

long

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_observation_longitude.jpg'),
  dpi = 300)

longlat <-
  my_species_sf %>%
  filter(
    x >= -86,
    x <= -82,
    y >= 8,
    y <= 12) %>%
  pivot_longer(
    c(x, y),
    names_to = 'axis',
    values_to = 'coordinates') %>%
  mutate(
    axis = factor(
      axis,
      levels = c('x', 'y'))) %>%
  ggplot(aes(x = coordinates)) +
  geom_density(fill = 'salmon') +
  facet_wrap(
    ~ axis,
    scales = 'free') +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = 'Densidad de observaciones por latitud y longitud',
    x = 'Coordenadas',
    y = 'Densidad') +
  theme_classic()

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_density_long-lat.jpg'),
  dpi = 300)

# Decline y recuperacion por canton ---------------------------------------

first_observation <-
  my_species_sf %>%
  group_by(canton) %>%
  mutate(year = year(date)) %>%
  filter(year == min(year)) %>%
  ggplot(
    aes(
      x = reorder(canton, desc(year)),
      y = year)) +
  geom_segment(
    aes(
      x = reorder(canton, desc(year)),
      xend = canton,
      y = 2022,
      yend = year),
    color = "#6f8faf") +
  geom_point(
    color = "#6f8faf",
    size = 4) +
  coord_flip() +
  labs(
    x = 'County',
    y = 'Year') +
  theme_classic()

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_first_observation.jpg'),
  dpi = 300)


all_observations <-
  my_species_sf %>%
  group_by(canton) %>%
  mutate(year = year(date)) %>%
  ggplot(
    aes(
      x = reorder(canton, desc(year)),
      y = year)) +
  geom_segment(
    aes(
      x = reorder(canton, desc(year)),
      xend = canton,
      y = 2022,
      yend = year),
    color = "cornflowerblue") +
  geom_point(
    colour = 'black',
    fill = "cornflowerblue",
    size = 4,
    shape = 21) +
  coord_flip() +
  labs(
      x = 'County',
    y = 'Year') +
  theme_classic()

ggsave(
  paste0(
    'output/figures/',
    my_species,
    '_all_observations.jpg'),
  dpi = 300)
