

# Curation Script ---------------------------------------------------------

# Dataset: ReefSYN
# Location: Guarapari
# Curator: Cher Chow

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(stringr)
require(readxl)
require(sf)
require(lubridate)
require(ggplot2)
require(rnaturalearth)

rm(list=ls())
dataID <- '134_richardson_lirs'

# make sure your working directory is set before running these lines
file = 'dataset_originals/134_richardson_lirs/FishData_CherChow_LizardIsland_2015-2018.xlsx'
dt <- read_excel(file, sheet = 1)
sampling  <- read_excel(file, sheet = 2)
coords  <- read_excel(file, sheet = 3)

# Structure org ====================================================================================

sampling$Replicate <- str_remove(sampling$Replicate, '^Post\\.')
sampling$Replicate <- str_replace(sampling$Replicate, '^Mix\\s2', 'Mix')
dt$Transect_ID <- str_replace(dt$Transect_ID, '^Mix\\s', 'Mix')
sampling <- sampling %>% mutate(Year = year(Survey_date), Month = month(Survey_date),
  Day = day(Survey_date), Transect_ID = paste(Replicate, Period, sep = '.'))
n_distinct(sampling$Transect_ID) == n_distinct(dt$Transect_ID)
dt <- sampling %>% select(Depth_m, Transect_ID, Year, Month, Day) %>%
  left_join(dt, ., by = "Transect_ID") %>%
  select(!c(Habitat, Period, Abundance))

## Function if coordinates are ever in degree minutes seconds
# angle input can read these formats
# 00º 00" 00
# 00 00 00
# 00º00"00

angle2dec <- function(angle) {
  angle <- as.character(angle)
  
  if (sum(str_detect(angle, '\\s*[:upper:]$')+0) > 0) {
    angle <- str_remove(angle, '\\s*[:upper:]$')
    warning("You passed coordinates containing N, E, S, or W. Stringr may have accounted for it but double check your outputs")
  }
  angle <- str_remove_all(angle, '([:punct:]|[:symbol:])\\s*[A-Z]$') 
  
  # use punctuation marks and symbols that aren't decimal points to split degrees minutes seconds apart
  x <- do.call(rbind, str_split(angle, '[^\\d\\.]'))
  if (ncol(x) == 3) {
    x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  } else {
    x <- apply(x, 1L, function(y) {
      y <- as.numeric(y)
      y[1] + y[2]/60
    })
  }
  return(x)
}

coords$Latitude <- angle2dec(coords$Latitude) * -1
coords$Longitude <- angle2dec(coords$Longitude)

dt <- coords %>% select(Site, Latitude, Longitude) %>%
  left_join(dt, ., by = 'Site')

# Structure check ==================================================================================

summary(dt)
str(dt)
# Abundance and/or length, latitude and longitude numeric? Y
# Taxonomic fields must be characters or factors? Y

# rename
dt <- rename(dt, Abundance = Density, Length = Length_cm, Depth = Depth_m)

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(is.na(dt$Abundance)) == 0 # any NAs
dt %>% filter(is.na(Abundance)) %>% View
dt  <- dt %>% filter(is.na(Abundance) == F)
sum(dt$Abundance == 0) == 0 # zeroes
sum(dt$Abundance == '') == 0 # blanks

sum(is.na(dt$Length)) == 0 # any NAs
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

# the locations look like they make sense
rm(points, world, proj)

# Taxonomic field check ---------------------------------------------------

require(taxize)

is.na(dt$Species) |> sum() == 0
(dt$Species == '') |> sum() == 0
is.na(dt$Family) |> sum() == 0

# validate
dt %>% distinct(Family, Species) %>% arrange(Family, Species) %>% View
dt$Species <- str_replace(dt$Species, 'sp+$', 'sp')
dt <- dt %>% mutate(taxon_resolution = case_when(str_detect(dt$Species, '\\ssp$') ~ 'genus',
.default = 'species'))
dt_sp <- dt %>% filter(taxon_resolution == 'species') %>% distinct(Species) %>% arrange(Species)
dt_sp$worms_id <- get_wormsid(dt_sp$Species, marine_only = T, fuzzy = T, messages = F)
dt_sp$worms_id[59] <- 218948
temp <- id2name(dt_sp$worms_id)
temp  <- bind_rows(temp[1:length(temp)])
dt_sp$worms_id <- as.numeric(dt_sp$worms_id)
dt_sp <- temp %>% mutate(id = as.numeric(id))  %>% 
  left_join(dt_sp, ., by = c('worms_id' = 'id'))

dt <- dt_sp %>% rename(worms_valid = name) %>% select(Species, worms_valid) %>%
  left_join(dt, dt_sp, by = 'Species')

dt$worms_valid[which(is.na(dt$worms_valid))] <- dt$Species[which(is.na(dt$worms_valid))]
dt$genus_valid <- word(dt$worms_valid, 1)
dt$Species <- NULL

# Check sampling effort + structure --------------------------------------------------------

dt %>% distinct(Site, Transect_ID) %>%
  group_by(Site) %>% summarise(transectN = n_distinct(Transect_ID)) %>% View

dt %>% group_by(Site) %>% summarise(yearN = n_distinct(Year)) %>% View

# Prepare raw data --------------------------------------------------------

View(dt)
dt$Transect_ID <- str_replace_all(dt$Transect_ID, '\\.', '_')
dt_final <- dt %>% mutate(DataID = dataID, 
  SamplingEventID = paste(Year, Transect_ID, sep = '_')) %>% 
  select(Abundance, Length, Family, genus_valid, worms_valid, taxon_resolution, SamplingEventID,
         Latitude, Longitude, Depth, Day, Month, Year, DataID) %>%
  rename(family_valid = Family) %>%
  arrange(Year, Month, Day, SamplingEventID, family_valid, genus_valid, worms_valid)

View(dt_final)

# Export final ------------------------------------------------------------

readr::write_csv(dt_final, paste0('dataset_outputs/', dataID, '_obs.csv'))

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
