

# Curation Script ---------------------------------------------------------

# Dataset: ReefSYN
# Location: Guarapari
# Curator: Cher Chow

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(stringr)
require(sf)
require(lubridate)
require(ggplot2)
require(rnaturalearth)

rm(list=ls())
dataID <- '135_emslie_gbraims'

load('dataset_originals/135_emslie_aims/fish_for_cher.RData')
dt <- fish_for_cher
rm(fish_for_cher)
summary(dt)
head(dt, 200) %>% View

# needs to be split at 2021

# Structure org ====================================================================================

# renames and remove unnecessary columns first
dt <- dt %>% select(ABUNDANCE, LENGTH, FAMILY, GENUS, SPECIES, P_CODE, FULLREEF_ID, VISIT_NO, 
  SITE_NO, SITE_LAT, SITE_LONG, TRANSECT_NO, REPORT_YEAR, SAMPLE_DATE, DEPTH, REPORT_YEAR, 
  SAMPLE_DATE) %>%
  rename(Year = REPORT_YEAR, Latitude = SITE_LAT, Longitude = SITE_LONG)
colnames(dt) <- str_to_title(colnames(dt))

# parse dates
dt$Sample_date <- as.Date(dt$Sample_date, format = '%d-%b-%Y')
dt <- dt %>% mutate(Month = month(Sample_date), Day = day(Sample_date))
dt$Sample_date <- NULL

# Structure check ==================================================================================

summary(dt)
str(dt)
# Abundance and/or length, latitude and longitude numeric? Y
# Taxonomic fields must be characters or factors? Y

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

nrow(dt)
dt <- dt %>% filter(Abundance > 0)
sum(is.na(dt$Abundance)) == 0
sum(dt$Abundance == 0) == 0 # zeroes
sum(dt$Abundance == '') == 0 # blanks

sum(is.na(dt$Length)) == 0 # any NAs
dt %>% filter(is.na(Length))
dt <- dt %>% filter(!is.na(Length))
sum(dt$Length == 0) == 0 # zeroes
sum(dt$Length == '') == 0 # blanks

summary(dt$Abundance)
summary(dt$Length) # range seems reasonable
dt %>% filter(Length > 70) %>% View
# fine 

# YEAR MONTH DAY
summary(dt)
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# good

proj = '+proj=eqearth +lon_0=160 +x_0=0 +y_0=0 +R=6371008.7714 +units=m +no_defs +type=crs'
points <- st_as_sf(dt %>% distinct(Longitude, Latitude, Year), coords = c("Longitude", "Latitude"), crs = 4326)
world <- ne_countries(scale = 'medium', returnclass = 'sf') %>% st_make_valid %>% 
  st_break_antimeridian(., lon_0 = 160) # run twice and the error should be gone
ggplot() +
  geom_sf(data = world, aes(group = admin), color='grey40', fill='grey95') +
  geom_sf(data = points, aes(fill = Year), shape = 21, size = 2) +
  guides(fill = 'none') + 
  coord_sf(crs = proj, ylim = c(-4366274.76, 4366274.76)) +
  theme_minimal()

  ggplot() +
  geom_sf(data = world, aes(group = admin), color='grey40', fill='grey95') +
  geom_sf(data = points, aes(fill = Year), shape = 21, size = 2) +
  guides(fill = 'none') + 
  coord_sf(crs = 4326, ylim = c(min(dt$Latitude) - 1, max(dt$Latitude) + 1),
    xlim = c(min(dt$Longitude) - 1, max(dt$Longitude) + 1)) +
  theme_minimal()

# the locations look like they make sense
rm(points, world, proj)

# Taxonomic field check ---------------------------------------------------

require(taxize)

is.na(dt$Species) |> sum() == 0
(dt$Species == '') |> sum() == 0
is.na(dt$Family) |> sum() == 0


# validate
dt %>% distinct(Family, Genus, Species) %>% arrange(Family, Genus, Species) %>% View
dt$Species <- str_replace(dt$Species, 'spp', 'sp')
dt$genus_sp <- paste(dt$Genus, dt$Species, sep = ' ')
dt <- dt %>% mutate(taxon_resolution = case_when(str_detect(dt$genus_sp, 'sp$') ~ 'genus',
.default = 'species'))
dt_sp <- dt %>% filter(taxon_resolution == 'species') %>% distinct(genus_sp) %>% arrange(genus_sp)
dt_sp$worms_id <- get_wormsid(dt_sp$genus_sp, marine_only = T, fuzzy = T, messages = F)
dt_sp %>% filter(is.na(worms_id)) %>% View
dt_sp$worms_id[dt_sp$genus_sp == 'Atherinomorus rogaa'] <- 218168
dt_sp$worms_id[dt_sp$genus_sp == 'Chlorurus spirulus'] <- 712772
dt_sp$worms_id[dt_sp$genus_sp == 'Turrum gymnostethus'] <- 218392
dt_sp$worms_id[dt_sp$genus_sp == 'Plectorhinchus picus'] <- 218555


temp <- id2name(dt_sp$worms_id)
temp  <- bind_rows(temp[1:length(temp)])
dt_sp$worms_id <- as.numeric(dt_sp$worms_id)
dt_sp <- temp %>% mutate(id = as.numeric(id))  %>% 
  left_join(dt_sp, ., by = c('worms_id' = 'id'))

dt <- dt_sp %>% rename(worms_valid = name) %>% select(genus_sp, worms_valid) %>% distinct %>%
  left_join(dt, dt_sp, by = 'genus_sp')

dt$worms_valid[which(is.na(dt$worms_valid))] <- dt$genus_sp[which(is.na(dt$worms_valid))]
dt$genus_valid <- word(dt$worms_valid, 1)
dt$Species <- NULL
dt$Genus <- NULL
dt$genus_sp <- NULL
dt$Family[dt$Family == 'Scaridae'] <- 'Labridae'

# Check sampling effort + structure --------------------------------------------------------

str(dt)
dt %>% distinct(Fullreef_id, Site_no, Transect_no, Year) %>%
  group_by(Year, Fullreef_id) %>% summarise(transectN = n()) %>% View
# only 2 anomaly reef-years
dt %>% group_by(Site) %>% summarise(yearN = n_distinct(Year)) %>% View

# Prepare raw data --------------------------------------------------------

View(dt)
dt_final <- dt %>% mutate(DataID = dataID, 
  SamplingEventID = paste(Year, Fullreef_id, Site_no, Transect_no, sep = '_')) %>% 
  select(Abundance, Length, Family, genus_valid, worms_valid, taxon_resolution, SamplingEventID,
         Latitude, Longitude, Depth, Day, Month, Year, DataID) %>%
  rename(family_valid = Family) %>%
  arrange(Year, Month, Day, SamplingEventID, family_valid, genus_valid, worms_valid)
Transect_ID
View(dt_final)

# Export final ------------------------------------------------------------

# split and export
dt_final %>% filter(Year < 2021) %>%
  mutate(DataID = paste0(DataID, '_method1')) %>%
  readr::write_csv(., paste0('dataset_outputs/', dataID, 'method1_obs.csv'))

dt_final %>% filter(Year >= 2021) %>%
  mutate(DataID = paste0(DataID, '_method2')) %>%
  readr::write_csv(., paste0('dataset_outputs/', dataID, 'method2_obs.csv'))


# Spatial calculations ------------------------------------------------

# load libraries
library(sf)
library(clipr)

# 1. Convert data points into point spatial object
# assumes your rawdata dataframe is called rawdata
dt_coord <- dt_final %>% distinct(DataID, Longitude, Latitude) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
  group_by(DataID) %>%
  summarise(Latitude = st_union(geometry) %>% st_centroid %>% unlist %>% .[2],
    Longitude = st_union(geometry) %>% st_centroid %>% unlist %>% .[1]) %>%
  st_drop_geometry()
