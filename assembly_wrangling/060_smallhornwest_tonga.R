

# Curation Script ---------------------------------------------------------

# Dataset: Benthic and reef fish surveys from the Kingdom of Tonga in 2016-2018 (375 sites)
# Location: Tonga

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(tidytable)
require(ggplot2)
require(readr)
require(stringr)
require(lubridate)
require(readxl)

rm(list=ls())
dataid <- '060_smallhornwest_tonga'

# make sure your working directory is set before running these lines
dt <- read_excel('dataset_originals/060_smallhornwest_tonga/tonga_reef_fish_surveys_2017_2018_fishbelt_25_05_28.xlsx', sheet = 4)
dt2 <- read_excel('dataset_originals/060_smallhornwest_tonga/tonga_reef_fish_surveys_2023_2024_fishbelt_25_05_28.xlsx', sheet = 4)

colnames(dt)
dt <- bind_rows(dt, dt2) %>% 
  select(Site, Latitude, Longitude, Year, Month, Day, 
  Depth, starts_with('Transect'), `Fish family`, `Fish genus`, `Fish taxon`, Size, Count)
rm(dt2)

# transform to counts to account for 2 pass method
dt <- dt %>% filter(!`Transect width` == 'Mixed: < 10cm @ 2m, >= 10cm @ 5m') %>%
  mutate(Abundance = case_when(`Transect width` == '5m' ~ Count,
                               `Transect width` == '2m' ~ Count*2.5,
                               `Transect width` == '1m' ~ Count*5))
unique(dt$`Transect length surveyed`)
dt$Abundance <- with(dt, (Abundance/`Transect length surveyed`)*50)
# densities for 50 * 5 m
colnames(dt) <- str_replace_all(colnames(dt), '\\s', '\\_')
dt <- dt %>% rename(Family = Fish_family, Length = Size) %>% select(-Count)
summary(dt)

# Structure check ---------------------------------------------------------

nrow(dt) # check dimensions
# 80985 rows
str(dt) # check structure


# logical?
summary(dt[,c('Year', 'Month', 'Day')]) # yes

# Primary field check -----------------------------------------------------

# ABUNDANCES
dt %>% filter(Size > 100) %>% View

# LAT LONG
library(rnaturalearth)
proj = '+proj=eqearth +lon_0=160 +x_0=0 +y_0=0 +R=6371008.7714 +units=m +no_defs +type=crs'
points <- st_as_sf(dt %>% distinct(Longitude, Latitude, Year), coords = c("Longitude", "Latitude"), crs = 4326)
world <- ne_countries(scale = 'medium', returnclass = 'sf') %>% st_make_valid %>% 
  st_break_antimeridian(., lon_0 = 160)
ggplot() +
  geom_sf(data = world, aes(group = admin), color='grey40', fill='grey95') +
  geom_sf(data = points, aes(fill = Year), shape = 21, size = 2) +
  guides(fill = 'none') + 
  coord_sf(crs = proj, ylim = c(-4366274.76, 4366274.76)) +
  theme_minimal()

# the locations look like they make sense
rm(points, world, proj)

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
sum(is.na(dt$Fish_taxon) + 0) == 0
sort(unique(dt$Fish_taxon))

dt$Species <- word(dt$Fish_taxon, 2, -1)
dt$Species[is.na(dt$Species)] <- 'sp'
sort(unique(dt$Fish_genus))

dt <- dt %>% rename(Genus = Fish_genus)
dt %>% distinct(Family, Genus, Species) %>% filter(Species == 'sp') %>% View

# match with species table
if (exists('dt_sp') == F) {
  dt_sp <- read_csv('macro_reef_fish_sp.csv')
}

dt %>% distinct(Family, Genus, Species) %>% 
  anti_join(., dt_sp, by = c('Family', 'Genus', 'Species')) %>% View
# without family has better matches
  dt %>% distinct(Genus, Species) %>% 
  anti_join(., dt_sp, by = c('Genus', 'Species')) %>% View

# match with the species table and then fill in validation gaps
dt <- dt_sp %>% distinct(Family, Genus, Species, worms_valid, fishbase_valid, .keep_all = T) %>%
select(taxaID, Family, Genus, Species, genus_sp, worms_valid, fishbase_valid) %>%
left_join(dt, ., by = c('Family', 'Genus', 'Species'), relationship = 'many-to-one')

summary(dt)

sp <- dt %>% distinct(Family, Genus, Species, taxaID:fishbase_valid) %>% 
  filter(is.na(taxaID)) %>% select(Family, Genus, Species)
View(sp)
sp <- dt_sp %>% distinct(Genus, Species, worms_valid, fishbase_valid, .keep_all = T) %>%
  select(taxaID, Genus, Species, genus_sp, worms_valid, fishbase_valid) %>%
  left_join(sp, ., by = c('Genus', 'Species'))
sp <- sp %>% filter(!is.na(taxaID))

for (i in 1:nrow(sp)) {
  dt$taxaID[dt$Genus == sp$Genus[i] & dt$Species == sp$Species[i]] <- sp$taxaID[i]
}
dt$taxaID[dt$Family == 'Scombridae' & dt$Species == 'sp'] <- 3327
dt$taxaID[dt$Family == 'Scaridae' & dt$Species == 'sp'] <- 3248

dt %>% distinct(Family, Genus, Species, taxaID:fishbase_valid) %>% 
  filter(is.na(taxaID)) %>% select(Family, Genus, Species)

summary(dt)
rm(sp)

dt$Site <- str_remove_all(dt$Site, '\\s')

# Prepare raw data --------------------------------------------------------

# check sampling structure
dt %>% group_by(Site) %>% summarise(transectN = n_distinct(Transect_number), yearN = n_distinct(Year)) %>% View
# not actually a time series

colnames(dt)
dt_final <- dt %>% mutate(DataID = dataid,
                          SamplingEventID = paste(Year, Month, Day, Site, Transect_number, sep = '_')) %>% 
  select(Abundance, Length, Family, Genus, Species, SamplingEventID, 
           Latitude, Longitude, Depth, Day, Month, Year, DataID)

# Export final ------------------------------------------------------------

write_csv(dt_final, paste0('dataset_outputs/', dataid, '_obs.csv'))

# Convex Hull for centroid ------------------------------------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_final %>% distinct(Longitude, Latitude) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

# 2. Calculate convex hull, area and centroid
centroid <- dt_coord %>% st_centroid() %>% unlist # get centroid
write_clip(centroid[c(2,1)]) # copy as lat-long
