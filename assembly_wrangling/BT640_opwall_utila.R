

# Curation Script ---------------------------------------------------------

# Dataset: Blue Alliance reef fish monitoring
# Location: Philippines
# Curator: Cher Chow

# Set up ------------------------------------------------------------------
# load the necessary packages
require(dplyr)
require(stringr)
require(readr)
require(readxl)
require(sf)
require(lubridate)
require(ggplot2)
require(rnaturalearth)

rm(list=ls())
dataID <- 'BT640_opwall_utila'

# make sure your working directory is set before running these lines
dt <- read_excel('dataset_originals/BT640_opwall_utila/Honduras_SVS_MASTER_no_formulae.xlsx')
site <- read_csv('dataset_originals/BT640_opwall_utila/honduras_sitecodes.csv')

str(site)
str(dt)

site <- site %>% mutate(Latitude = str_extract(coordinates, '(?<=(N|S)\\s)[0-9\\.]{2,}'),
  Longitude = str_extract(coordinates, '(?<=(E|W)\\s)[0-9\\.]{2,}'))

dt <- site %>% select(sitecode, Latitude, Longitude) %>%
  left_join(dt, ., by = c('Site' = 'sitecode')) %>%
  select(!c(Juvenile, Guild, `Biomass(g)`, Length_check))

# Structure check ---------------------------------------------------------

summary(dt)
nrow(dt) # check dimensions
str(dt)

# Abundance and/or length, latitude and longitude numeric
# Taxonomic fields must be characters or factors

dt$Latitude <- dt$Latitude |> as.numeric()
dt$Longitude <- dt$Longitude |> as.numeric()

# renames
dt <- rename(dt, Abundance = Number, Length = `Length(mm)`)

# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
sum(is.na(dt$Abundance)) == 0 # any NAs
sum(dt$Abundance == 0) == 0 # zeroes
sum(dt$Abundance == '') == 0 # blanks

sum(is.na(dt$Length)) == 0 # any NAs
sum(dt$Length == 0) == 0 # zeroes
sum(dt$Length == '') == 0 # blanks

summary(dt$Abundance)
summary(dt$Length) # range seems reasonable
dt$Length <- dt$Length / 10 # convert to cm
dt %>% filter(Length > 70) %>% View # yep, ok

summary(dt)
# dates are annual
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# some NAs

# give Ensenada the centroid coordinates
coord <- dt %>% distinct(Latitude, Longitude) %>% filter(!is.na(Latitude)) %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
  st_union %>% st_centroid %>% st_coordinates %>% as.vector %>% .[c(2,1)]
dt$Latitude[is.na(dt$Latitude)] <- coord[1]
dt$Longitude[is.na(dt$Longitude)] <- coord[2]
rm(coord)
dt$Longitude <- dt$Longitude * -1

proj = '+proj=eqearth +lon_0=160 +x_0=0 +y_0=0 +R=6371008.7714 +units=m +no_defs +type=crs'
points <- dt %>% filter(is.na(Latitude) == F) %>%
  distinct(Longitude, Latitude, Year) %>% 
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)
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

dt %>% distinct(Family, Genus, Species) %>%
  arrange(Family, Genus, Species) %>% View

dt$Species[dt$Species == 'spp'] <- 'sp'

# non reals
sum(dt$Species == '') == 0
dt$Full_name <- NULL
# wow so clean

# Sampling effort + sampling event IDs  -----------------------------------

str(dt)
dt %>% group_by(Location, Site, Year) %>% 
  summarise(replicate_n = n_distinct(Transect)) %>% View


# usually 6 transects per year per site, some with 3. will let rarefying deal with that

# Prepare raw data --------------------------------------------------------

View(dt)

dt_final <- dt %>% mutate(DataID = dataID, Day = NA, Month = NA,
  SamplingEventID = paste(Year, Site, Transecht, sep = '_')) %>% 
  select(Abundance, Length, Family, Genus, Species, SamplingEventID,
         Latitude, Longitude, Depth, Day, Month, Year, DataID) %>%
  arrange(SamplingEventID, Family, Genus, Species)

View(dt_final)

# Export final ------------------------------------------------------------

write_csv(dt_final, paste0('dataset_outputs/', dataID, '_obs.csv'))

# Spatial calculations ------------------------------------------------

