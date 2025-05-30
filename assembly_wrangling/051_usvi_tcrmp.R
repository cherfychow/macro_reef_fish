

# Curation Script ---------------------------------------------------------

# Dataset: U.S. Virgin Islands Territorial Coral Reef Monitoring Program Database (USVI-TCRMP)
# Location: US Virgin Islands
# Curator: Cher Chow

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidytable)
require(stringr)
require(sf)
require(readxl)
require(readr)

rm(list=ls())
dataID <- '051_usvi_tcrmp'

dt <- read_excel('dataset_originals/051_usvi_tcrmp/TCRMP_Master_Fish_Census_Aug2022.xlsx', sheet = 4)
unique(dt$Period)
# annual, post hurricane, research cruise
dt_ws <- dt %>% filter(Period == 'WS', Metric == 'Abundance')
dt <- dt %>% filter(Period != 'WS', Metric == 'Abundance') %>%
  select(-CommonName, -TrophicGroup, -Total, -Metric)

# get Length
dt <- pivot_longer(dt, cols = matches('\\d\\-'), names_to = 'size_class', values_to = 'Abundance')
dt$Length_lwr <- str_extract(dt$size_class, '^\\d+') %>% as.numeric
dt$Length_upr <- str_extract(dt$size_class, '\\d+$') %>% as.numeric
dt$Length <- with(dt, Length_lwr + ((Length_upr - Length_lwr) / 2))

summary(dt)
dt$size_class <- NULL
dt$Period <- NULL

# merge in coordinate information

coords <- read_excel('dataset_originals/051_usvi_tcrmp/TCRMP_Master_Fish_Census_Aug2022.xlsx', sheet = 2)
sort(unique(dt$Location))
sort(unique(coords$Location))
replace <- c('castle' = 'Castle',
             'lang' = 'Lang',
             'shoal' = 'Shoal')
dt$Location <- str_replace_all(dt$Location, replace)
sort(unique(dt$Location)) == sort(unique(coords$Location))

dt <- coords %>% select(Location, Latitude, Longitude, Depth) %>%
  left_join(dt, ., by = 'Location')

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(is.na(dt$Abundance)+0)
min(dt$Abundance, na.rm=T) # no zeroes? N
dt <- dt %>% filter(Abundance > 0)
sum(is.na(dt$Length)+0) # 237 NAs for length
min(dt$Length, na.rm=T) # no zeroes? Y
range(dt$Length)

# YEAR MONTH DAY
# already checked

# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
summary(dt[c('Latitude', 'Longitude')]) # good

library(rnaturalearth)
proj = '+proj=eqearth +lon_0=160 +x_0=0 +y_0=0 +R=6371008.7714 +units=m +no_defs +type=crs'
points <- st_as_sf(dt %>% distinct(Longitude, Latitude, Year), coords = c("Longitude", "Latitude"), crs = 4326)
world <- ne_countries(scale = 'medium', returnclass = 'sf')
world <- st_make_valid(world)
world <- st_break_antimeridian(world, lon_0 = 160)
require(ggplot2)
ggplot() +
  geom_sf(data = world, aes(group = admin), color='grey40', fill='grey95') +
  geom_sf(data = points, aes(fill = Year), shape = 21, size = 2) +
  guides(fill = 'none') + 
  coord_sf(crs = proj, ylim = c(-4366274.76, 4366274.76)) +
  theme_minimal()

# the locations look like they make sense
rm(points, world, proj)

# Taxonomic field check ---------------------------------------------------

sort(unique(dt$ScientificName))
dt <- dt %>% mutate(Genus = word(ScientificName, 1) %>% str_to_title, 
  Species = word(ScientificName, 2, -1) %>% str_remove(., '\\.'), 
  ScientificName = paste(Genus, Species, sep = ' '))

# No NAs in taxonomic fields, remove all non-organism records
dt %>% distinct(Genus, Species) %>% filter(is.na(Genus), is.na(Species))

# validate
dt$Family <- NA
dt_sp <- read_csv('dt_species.csv')
dt %>% distinct(Genus, Species, ScientificName) %>% 
  inner_join(., dt_sp, by = c('Genus', 'Species')) %>% View

# match with the species table and then fill in validation gaps

dt <- dt_sp %>% distinct(Genus, Species, worms_valid, fishbase_valid, .keep_all = T) %>%
select(taxaID, Genus, Species, genus_sp, worms_valid, fishbase_valid) %>%
left_join(dt, ., by = c('Genus', 'Species'), relationship = 'many-to-one')
summary(dt)
# looks good, just need to add one species record
dt %>% filter(is.na(taxaID)) %>% distinct(Genus, Species)
dt_sp %>% filter(genus_valid == 'Kyphosus') %>% View
dt <- dt %>% select(-ends_with('valid'), -genus_sp)
dt$taxaID[is.na(dt$taxaID)] <- max(dt_sp$taxaID) + 1

sp_add <- tidytable(
  taxaID = 4837,
  Family = NA,
  Genus = 'Kyphosus',
  Species = 'incisor',
  taxon_resolution = 'species',
  genus_sp = 'Kyphosus incisor',
  worms_valid = 'Kyphosus incisor',
  genus_valid = 'Kyphosus',
  family_valid = 'Kyphosidae',
  fishbase_valid = 'Kyphosus incisor'
)

dt_sp <- bind_rows(dt_sp, sp_add)
# write_csv(dt_sp, file = 'macro_reef_fish_sp.csv')

# Sample check --------------------------------------------------------

dt %>% group_by(Location, Year, Month) %>% summarise(nTransect = n_distinct(Transect)) %>% View

# Prepare raw data --------------------------------------------------------

dt_final <- dt %>% mutate(DataID = dataID, 
SamplingEventID = paste(Year, Month, Location, Transect, sep = '_'), Day = NA) %>% 
  relocate(Abundance, Length, Length_lwr, Length_upr, Family, Genus, Species, SamplingEventID, 
           Latitude, Longitude, Depth, Day, Month, Year, DataID)

# Export final ------------------------------------------------------------

write_csv(dt_final, paste0('dataset_outputs/', dataID, '_obs.csv'))

# Spatial calculations ------------------------------------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_final %>% distinct(Longitude, Latitude) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_convex_hull() %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long

