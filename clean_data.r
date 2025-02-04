

require(dplyr)
require(readr)
require(taxize)
require(rfishbase)
require(worrms)
require(stringr)
require(purrr)

# load in data
dt_assemblage <- read_csv('macro_reef_fish_obs.csv')
gc() # keeping memory usage low

summary(dt_assemblage)

# some datasets already validated
# isolate, clean, then remerge later
dt_valid <- dt_assemblage %>% filter(is.na(genus_valid) == F)
dt_notclean <- dt_assemblage %>% filter(is.na(genus_valid))
nrow(dt_valid) + nrow(dt_notclean) == nrow(dt_assemblage)
rm(dt_assemblage)

# ==============================================================================
# GENERAL CHECKS
# ==============================================================================

# make a species table
allsp <- dt_notclean %>% distinct(Family, Genus, Species) %>% arrange(Family, Genus, Species)
allsp$taxaID <- 1:nrow(allsp)
# make a taxonomic ID key
dt_notclean <- left_join(dt_notclean, allsp, by = c('Family', 'Genus', 'Species'))

allsp %>% arrange(Family, Genus, Species) %>% View
sort(unique(allsp$Family))
sort(unique(allsp$Genus))

# first check format, genus and epithet split
allsp %>% filter(str_detect(Genus, '\\s'))
# error parsing the encoded space
allsp$Genus[str_which(allsp$Genus, '\\s')]  <- str_replace(allsp$Genus[str_which(allsp$Genus, '\\s')], '\\s', ' ')
# split out epithet
allsp$Species[str_which(allsp$Genus, '\\s')] <- word(allsp$Genus[str_which(allsp$Genus, '\\s')], 2)
# genus
allsp$Genus[str_which(allsp$Genus, '\\s')] <- word(allsp$Genus[str_which(allsp$Genus, '\\s')], 1)

# ==============================================================================
#  POST HOC DATE FIXES
# ==============================================================================



# missing dates, year

dt_assemblage %>% filter(is.na(Year)) %>% distinct(DataID)


dt_assemblage %>% filter(str_detect(DataID, '^028')) %>% distinct(DataID) %>% View
View(dt_assemblage %>% filter(str_detect(DataID, '^028')) %>% distinct(DataID, Year, Month))
dt_assemblage$Year[dt_assemblage$DataID == '028_caribbean_cuba'] <- 2009
dt_assemblage$Year[dt_assemblage$DataID == '028_eastern_atlantic_cverde'] <- 2009
dt_assemblage$Year[dt_assemblage$DataID == '028_eastern_atlantic_stome'] <- 2006
dt_assemblage$Year[dt_assemblage$DataID == '028_mexico'] <- 2013
dt_assemblage$Month[dt_assemblage$DataID == '028_mexico'] <- 10
dt_assemblage$Year[dt_assemblage$DataID == '028_curacao'] <- 2013
dt_assemblage$Year[dt_assemblage$DataID == '028_loyalty'] <- 2012
dt_assemblage$Year[dt_assemblage$DataID == '028_pitcairn'] <- 2012
dt_assemblage$Year[dt_assemblage$DataID == '028_agreen_palau'] <- 2000
dt_assemblage$Year[dt_assemblage$DataID == '028_agreen_raja_ampat_2012'] <- 2009
dt_assemblage$Year[dt_assemblage$DataID == '028_agreen_samoa'] <- 1995
dt_assemblage$Year[dt_assemblage$DataID == '028_agreen_solomon_2008'] <- 2008
dt_assemblage$Year[dt_assemblage$DataID == '028_flores_1993_michel'] <- 1993
dt_assemblage$Year[dt_assemblage$DataID == '028_fiji_biomass_2013a'] <- 2003
dt_assemblage$Year[dt_assemblage$DataID == '028_seychelles_alan_2017'] <- 2015
dt_assemblage$Month[dt_assemblage$DataID == '028_seychelles_alan_2017'] <- 3
dt_assemblage$Year[dt_assemblage$DataID == '028_revillagigedo_alan_2017'] <- 2016
dt_assemblage$Month[dt_assemblage$DataID == '028_revillagigedo_alan_2017'] <- 4
dt_assemblage$Year[dt_assemblage$DataID == '028_mozambique_alan_2017'] <- 2014
dt_assemblage$Month[dt_assemblage$DataID == '028_mozambique_alan_2017'] <- 4
dt_assemblage$Year[dt_assemblage$DataID == '028_easter_salaz'] <- 2011
dt_assemblage$Month[dt_assemblage$DataID == '028_easter_salaz'] <- 3
dt_assemblage$Year[dt_assemblage$DataID == '028_cocos'] <- 2009
dt_assemblage$Month[dt_assemblage$DataID == '028_cocos'] <- 9
dt_assemblage$Year[dt_assemblage$DataID == '027_friedlander_palau'] <- 2014
dt_assemblage$Month[dt_assemblage$DataID == '027_friedlander_palau'] <- 9
dt_assemblage$Month[dt_assemblage$DataID == '028_clipperton_alan_2017'] <- 3
dt_assemblage$Year[dt_assemblage$DataID == '028_clipperton_alan_2017'] <- 2016
dt_assemblage$Month[dt_assemblage$DataID == '028_new_caledonia_biomass_2013c1'] <- 11
dt_assemblage$Month[dt_assemblage$DataID == '028_fpolynesia_biomass_2013'] <- 11
dt_assemblage$Year[dt_assemblage$DataID %in% c('028_fpolynesia_biomass_2013', '028_new_caledonia_biomass_2013c1', '028_tonga_biomass_2013')] <- NA


dt_assemblage <- dt_assemblage %>% mutate(Year = case_when(.default = Year,
                                                           DataID == '017_brandl_uaecryptic' ~ 2018,
                                                           DataID == '033_jennings_fiji' ~ 1995,
                                                           DataID == '72_alvarezfilip_cozumel' ~ 2007,
                                                           DataID == '104_brandl_crypticflux_belize' ~ 2016,
                                                           DataID == '104_brandl_crypticflux_lizard' ~ 2018,
                                                           DataID == '104_brandl_crypticflux_moorea' ~ 2017),
                                          Month = case_when(.default = Month,
                                                            DataID == '72_alvarezfilip_cozumel' ~ 12,
                                                            DataID == '104_brandl_crypticflux_belize' ~ 3,
                                                            DataID == '104_brandl_crypticflux_lizard' ~ 2,
                                                            DataID == '104_brandl_crypticflux_moorea' ~ 10
                                          ))

patch <- read_csv('src/060_patch.csv')
colnames(dt_assemblage)
for (i in 1:nrow(patch)) {
  dt_assemblage[which(dt_assemblage$SamplingEventID == patch$SamplingEventID[i]), 9:11] <- list(patch$Day[i], patch$Month[i], patch$Year[i])
}


# ==============================================================================
# CLEAN GENUS
# ==============================================================================

allsp %>% filter(str_detect(Genus, 'ei$|dae$|nae$'))

allsp$Family[which(allsp$Genus == 'Blennioidei')] <- 'Blennioidei'
allsp$Genus[which(allsp$Genus == 'Blennioidei')] <- NA
allsp$Genus <- str_to_title(allsp$Genus)
sort(unique(allsp$Genus))
allsp %>% filter(str_detect(Genus, 'Blenny|Goby|fish$'))

allsp$Family[which(allsp$Genus == 'Blenny')] <- 'Blennioidei'
allsp$Genus[which(allsp$Genus == 'Blenny')] <- NA

allsp$Family[which(allsp$Genus == 'Squirrelfish')] <- 'Holocentridae'
allsp$Genus[which(allsp$Genus == 'Squirrelfish')] <- NA

allsp$Species[allsp$Genus == 'Scarinissp8'] <- 'sp8'
allsp$Genus[allsp$Genus == 'Scarinissp8'] <- 'Scarini'

# ==============================================================================
# CLEAN EPITHETS
# ==============================================================================

# allow open nomenclature + specified uncertains. no subspecies

# subspecies?
allsp %>% filter(str_detect(Species, '\\s|\\d')) %>% View
# fix specified uncertains first
# only manual fix what automatic validation can't
allsp <- allsp %>% 
  mutate(Species = case_when(str_detect(Species, '^\\d$') ~ paste0('sp', Species),
                             str_detect(Species, 'spp|sp\\.|species|^spies$') ~ 'sp',
                             .default = Species))

allsp %>% filter(str_detect(Species, '\\s|\\d')) %>% View

# subspecies handling
allsp$Species[which(str_detect(allsp$Species, '\\s') & str_detect(allsp$Species, '^sp') == F & str_detect(allsp$Species, 'cf\\.') == F)] <- word(allsp$Species[which(str_detect(allsp$Species, '\\s') & str_detect(allsp$Species, '^sp') == F & str_detect(allsp$Species, 'cf\\.') == F)], 1)

# specified uncertains
allsp %>% filter(str_detect(Species, '\\s|\\d|^sp.{0,2}')) %>% View
# e.g. sp cabo verde these are unique to a genus and a dataID.
allsp$Species <- str_replace(allsp$Species, '\\scabo\\sverde$|\\scuba$|\\scaribe$', '1')
allsp %>% filter(str_detect(Species, '^sp[a-z]$')) %>% View
allsp$Species[allsp$Species %in% c('spx', 'spd')] <- 'sp1'
allsp$Species[allsp$Species == 'spz'] <- 'sp2'

# omits
dt_notclean <- dt_notclean[-which(str_detect(dt_notclean$Species, 'crcp|franc|CRCP') & is.na(dt_notclean$Genus)),]

# ==============================================================================
# GENERAL UNCERTAINS
# ==============================================================================

allsp %>% filter(is.na(Genus) | Species == 'sp') %>% View
allsp %>% filter(is.na(Genus)) %>% View
# family level consistency. if it's not specified, Species should be empty
allsp[which(is.na(allsp$Genus) & allsp$Species == 'sp'),]
allsp$Species[which(is.na(allsp$Genus) & allsp$Species == 'sp')] <- NA

# ==============================================================================
# Generate taxon resolution column
# ==============================================================================

dt_notclean %>% filter(is.na(Family), is.na(Genus)) %>% View
# omit
dt_notclean <- dt_notclean[-with(dt_notclean, which(is.na(Family) & is.na(Genus))),]

allsp$taxon_resolution <- NA
# rules from most general to most specific
allsp$taxon_resolution[with(allsp, which(is.na(Family) == F & is.na(Genus)))] <- 'family'
allsp$taxon_resolution[with(allsp, which(is.na(Genus) == F & is.na(Species) == F))] <- 'species'
allsp$taxon_resolution[with(allsp, which(is.na(Genus) == F & str_detect(Species, '^sp$')))] <- 'genus'

# now the exceptions
allsp$taxon_resolution[with(allsp, which(Genus == 'Scarini'))] <- 'family'
allsp$taxon_resolution[with(allsp, which(is.na(Genus) & str_detect(Species, '^sp\\d+$')))] <- 'family specified'
allsp$taxon_resolution[with(allsp, which(is.na(Genus) == F & str_detect(Species, '^sp\\d+$')))] <- 'genus specified'
# Scarini special
allsp$taxon_resolution[with(allsp, which(Genus == 'Scarini' & str_detect(Species, '^sp\\d+$')))] <- 'family specified'
allsp %>% filter(!str_detect(Family, 'dae')) %>% View

# some coarser than family but we keep for richness purposes
allsp$Species[allsp$Genus == 'Teleostei'] <- NA
allsp$Genus[allsp$Genus == 'Teleostei'] <- NA
allsp$Family[with(allsp, which(str_detect(Family, 'dae') == F & is.na(Genus)))]
allsp$taxon_resolution[with(allsp, which(str_detect(Family, 'dae') == F & is.na(Genus)))] <- 'lowres'

# check
allsp %>% filter(taxon_resolution == 'species') %>% sample_n(50) %>% View
allsp %>% filter(taxon_resolution == 'genus') %>% sample_n(50) %>% View
allsp %>% filter(taxon_resolution == 'genus specified') %>% sample_n(30) %>% View
allsp %>% filter(taxon_resolution == 'family specified') %>% sample_n(10) %>% View
allsp %>% filter(taxon_resolution == 'lowres') %>% View

# ==============================================================================
# AUTOMATED SPECIES VALIDATION/SPELLCHECK
# ==============================================================================

dt_notclean %>% filter(Species == 'Red')
dt_notclean %>% filter(DataID == '060_smallhornwest_tonga', Genus == 'Apogon') %>% View
allsp$taxon_resolution[allsp$taxaID == 103] <- 'genus specified'
allsp$Species[allsp$Species == 'Red'] <- 'sp1'

# generate species list for checking
dt_spcheck <- allsp %>% # only certain, species level IDs for this stage
  filter(taxon_resolution == 'species' & str_detect(Species, '\\scf') == F) %>% 
  mutate(genus_sp = paste(Genus, Species, sep = ' ')) %>% 
  distinct(genus_sp) %>% arrange(genus_sp)

# get WoRMS ID to validate. GBIF as a backup
dt_spcheck$worms_id <- get_wormsid(dt_spcheck$genus_sp, searchtype = 'scientific', fuzzy = T,
                                   marine_only = T, messages = F)
wm_records_names
dt_spcheck %>% filter(is.na(worms_id)) %>% View
dt_spcheck$gbif_id <- NA
dt_spcheck$gbif_id[is.na(dt_spcheck$worms_id)] <- get_gbifid(dt_spcheck$genus_sp[is.na(dt_spcheck$worms_id)], messages = F, method = 'backbone')

# first round of catching unaccepted names
dt_spcheck %>% filter(is.na(worms_id) & is.na(gbif_id)) # ok only three to fix manually
# caught some misspellings
dt_spcheck <- dt_spcheck[-str_which(dt_spcheck$genus_sp, 'aff\\.'),]
allsp$Species[str_which(allsp$Species, 'cf[a-z]')] <- str_replace(allsp$Species[str_which(allsp$Species, 'cf[a-z]')], 'cf', 'cf. ')
dt_spcheck$genus_sp[str_which(dt_spcheck$genus_sp, 'cf[a-z]')] <- str_replace(dt_spcheck$genus_sp[str_which(dt_spcheck$genus_sp, 'cf[a-z]')], 'cf', 'cf. ')

# manual ID
# Menaphorus punticulatus will not be matched with traits 
dt_spcheck$worms_id[dt_spcheck$genus_sp == 'Calostomus coralinus'] <- 219076
dt_spcheck$worms_id[dt_spcheck$genus_sp == 'Eviota cf. latifasciata'] <- 278595
dt_spcheck$worms_id[dt_spcheck$genus_sp == 'Eviota cf. nebulosa'] <- 219459
dt_spcheck$worms_id[dt_spcheck$genus_sp == 'Ostorhinchus exostigma'] <- 712695
dt_spcheck <- dt_spcheck[-which(dt_spcheck$genus_sp == 'Menaphorus punticulatus'),]

# get names, identify valid vs synonyms
dt_spcheck <- dt_spcheck %>%
  mutate(across(ends_with('id'), as.numeric))
worms <- wm_record(dt_spcheck$worms_id |> na.omit())  %>% bind_rows # will take a loong time
gc()
dt_spcheck <- worms %>% select(AphiaID, scientificname, status, valid_name, class, family, genus) %>% 
  rename(worms_valid = valid_name, worms_status = status, worms_name = scientificname) %>% distinct %>% 
  left_join(dt_spcheck, ., by = c('worms_id' = 'AphiaID'))

dt_spcheck %>% filter(is.na(worms_id)) %>% View
gbif <- id2name(dt_spcheck$gbif_id |> na.omit(), db = 'gbif')
gbif <- bind_rows(gbif[1:length(gbif)])
dt_spcheck2 <- gbif %>% select(!rank) %>% 
  rename(gbif_name = name, gbif_status = status, gbif_id = id) %>% distinct %>% 
  mutate(gbif_id = as.numeric(gbif_id)) %>% 
  left_join(dt_spcheck, ., by = "gbif_id")
dt_spcheck <- dt_spcheck2
rm(dt_spcheck2)

# fill in the NAs
dt_spcheck$worms_valid[with(dt_spcheck, which(is.na(worms_valid) & str_detect(gbif_name, '\\s')))] <- dt_spcheck$gbif_name[with(dt_spcheck, which(is.na(worms_valid) & str_detect(gbif_name, '\\s')))]

# leftovers?
dt_spcheck %>% filter(is.na(worms_valid)) %>% View
# remove some from automated validation
dt_spcheck <- dt_spcheck %>% filter(!str_detect(genus_sp, 'hybrid|townsendi'))
dt_notclean %>% filter(Genus == 'Acanthochromis', Species == 'japonicus')

# manual validation for these
dt_spcheck <- dt_spcheck %>% 
  mutate(worms_valid = case_when(.default = worms_valid,
                                 genus_sp == "Acanthochromis japonicus" ~ 'Acanthurus japonicus',
                                 genus_sp == "Cantherhines maculatus" ~ 'Canthidermis maculata',
                                 genus_sp == "Chromis tricincta" ~ 'Chrysiptera tricincta',
                                 genus_sp == "Ecsenius flavus" ~ 'Plagiotremus flavus',
                                 genus_sp == "Enneanectes tutuilae" ~ 'Enneapterygius tutuilae',
                                 genus_sp == "Epinephelides guttatus" ~ 'Epinephelus guttatus',
                                 genus_sp == "Hypoplectrus tann" ~ 'Hypoplectrus randallorum',
                                 genus_sp == "Labroides manabei" ~ 'Labropsis manabei',
                                 genus_sp == "Pomacentrus margaritifer" ~ 'Pycnochromis margaritifer',
                                 genus_sp == "Ucla macrospilus" ~ 'Acanthemblemaria macrospilus'
                                 ))

dt_spcheck %>% filter(is.na(worms_valid)) %>% View
# double check

# ==============================================================================
# DISTINGUISH NON FISH + MERGE BACK IN WITH SP TABLE
# ==============================================================================

dt_spcheck %>% filter(class %in% c('Teleostei', 'Elasmobranchii') == F, is.na(worms_id) == F) %>% View
dt_spcheck <- dt_spcheck %>% mutate(remove = case_when(class %in% c('Teleostei', 'Elasmobranchii') == F & is.na(worms_id) == F ~ '1', .default = NA))

# merge in
allsp <- allsp %>% 
  mutate(genus_sp = case_when(taxon_resolution %in% c('genus', 'genus specified', 'species') ~ paste(Genus, Species, sep = ' '), .default = NA)) %>%
  left_join(., dt_spcheck[,c(`genus_sp`,`worms_valid`)], by = 'genus_sp')

# do these need spell checking
allsp %>% 
  filter(taxon_resolution %in% c('family specified', 'genus', 'genus specified', 'family')) %>%
  distinct(Family, Genus) %>% View

# hidden family level IDs
allsp %>% filter(str_detect(Genus, 'id$'))
allsp$taxon_resolution[str_detect(allsp$Genus, 'id$')] <- 'family'
allsp$Species[str_detect(allsp$Genus, 'id$')] <- NA
allsp$Genus[str_detect(allsp$Genus, 'id$')] <- NA

allsp <- allsp %>% filter(!family_valid %in% notfish)

# ==============================================================================
# VALIDATE AND MATCH GENERA + FAMILIES
# ==============================================================================

famgen <- dt_spcheck %>% mutate(genus = word(worms_valid, 1)) %>%
  distinct(genus, family)
famgen %>% filter(is.na(family)) %>% nrow
famgen %>% filter(is.na(family) == F) %>% nrow
n_distinct(famgen$genus)
# not all genera have family matches
famgen <- famgen %>% group_by(genus) %>% arrange(genus, family) %>% 
  mutate(n = 1:n()) %>% # intermediate order column
  filter(n == 1) %>% select(!n)
famgen %>% filter(is.na(family)) # only 2, no need to automate fetching families

famgen$family[famgen$genus == 'Diancistrus'] <- 'Bythitidae'
famgen$family[famgen$genus == 'Stathmonotus'] <- 'Chaenopsidae'
colnames(famgen) <- c('genus_valid', 'family_valid')
famgen$family_valid[famgen$family_valid == 'Scaridae'] <- 'Labridae'
famgen$family_valid[str_which(famgen$genus_valid, 'anthia')] # check anthias

# CLEAN UNCERTAIN GENERA

gen2 <- allsp %>% filter(is.na(worms_valid), is.na(Genus) == F) %>% distinct(Genus) %>%
  filter(Genus %in% famgen$genus_valid == F)
worms <- wm_records_names(name = gen2$Genus, marine_only = T, fuzzy = T) %>% bind_rows
gen2 %>% filter(Genus %in% worms$scientificname == F)
gen2 <- worms %>% select(scientificname, family, genus) %>% 
  rename(family_valid = family, genus_valid = genus) %>% 
  left_join(gen2, ., by = c('Genus' = 'scientificname'))

gen2$family_valid[gen2$Genus == 'Cirripects'] <- 'Blenniidae'
gen2$genus_valid[gen2$Genus == 'Cirripects'] <- 'Cirripectes'

gen2$family_valid[gen2$Genus == 'Mypristis'] <- 'Holocentridae'
gen2$genus_valid[gen2$Genus == 'Mypristis'] <- 'Myripristis'

gen2$family_valid[gen2$Genus == 'Macropharyngeodon'] <- 'Labridae'
gen2$genus_valid[gen2$Genus == 'Macropharyngeodon'] <- 'Macropharyngodon'

gen2$family_valid[gen2$Genus == 'Scarini'] <- 'Labridae'
gen2$genus_valid[gen2$Genus == 'Scarini'] <- 'Scarini'

gen2$family_valid[gen2$Genus == 'Entomacordus'] <- 'Blenniidae'
gen2$genus_valid[gen2$Genus == 'Entomacordus'] <- 'Entomacrodus'

gen2$family_valid[gen2$Genus %in% c('Menaphorus', 'Menephorus')] <- NA
gen2$genus_valid[gen2$Genus %in% c('Menaphorus', 'Menephorus')] <- 'Menaphorus'

gen2$family_valid[gen2$Genus == 'Scarideo'] <- 'Labridae'
gen2$genus_valid[gen2$Genus == 'Scarideo'] <- 'Calotomus'

allsp <- gen2 %>% select(!family_valid) %>% # only validated genera
  left_join(allsp, ., by = 'Genus') %>% # add in
  # for validate species, pick out validated genera
  mutate(genus_valid = case_when(is.na(worms_valid) == F ~ word(worms_valid, 1), .default = genus_valid))
# for uncertain genera that don't need fixing
allsp <- allsp %>% 
  mutate(genus_valid = case_when(is.na(genus_valid) & is.na(Genus) == F ~ Genus, .default = genus_valid))

allsp %>% filter(str_detect(taxon_resolution, 'family')) %>% distinct(Family) %>% View
allsp$Family[allsp$Family == 'Caesioinidae'] <- 'Caesionidae'

# fill in the valid name for ones that got excluded from automation
allsp %>% filter(taxon_resolution == 'species', is.na(worms_valid)) %>% View
allsp$worms_valid[with(allsp, which(taxon_resolution == 'species' & is.na(worms_valid)))] <- allsp$genus_sp[with(allsp, which(taxon_resolution == 'species' & is.na(worms_valid)))]
allsp$taxon_resolution[allsp$genus_sp == 'Malacoctenus aff.'] <- 'genus'

# merge in the family-genera links
allsp2 <- gen2 %>% select(ends_with('valid')) %>% bind_rows(famgen, .) %>% distinct %>%
  left_join(allsp, ., by = 'genus_valid')
View(allsp2)
rm(worms, gbif, dt_spcheck, leftover)

allsp <- allsp2
rm(allsp2)
# fill in for the family level records
allsp$family_valid[is.na(allsp$Family) == F & is.na(allsp$family_valid)] <- allsp$Family[is.na(allsp$Family) == F & is.na(allsp$family_valid)]

allsp %>% filter(is.na(family_valid)) %>% View
# use the family_valid field to leave out Menaphorus spp from trait matching

# save a copy in case
# allsp %>% relocate(taxaID, .before = Family) %>%
#   write_csv(., 'dt_species.csv')

# ==============================================================================
# MATCH IN FISHBASE NAMES FOR TRAIT MATCHING
# ==============================================================================

# needed for trait compilation

fb_valid <- allsp %>% filter(taxon_resolution == 'species') %>% 
  distinct(worms_valid) %>% arrange(worms_valid)
fb_sp <- species(species_list = fb_valid$worms_valid) # get fishbase valid species
fb_syn <- synonyms(species_list = fb_valid$worms_valid)
fb_valid <- left_join(fb_valid, fb_sp[1:2], by = c('worms_valid' = 'Species'))

# isolate species that can't match
fb_valid %>% filter(is.na(SpecCode)) %>% View
manuals <- fb_valid %>% filter(is.na(SpecCode)) %>% select(!SpecCode)
manuals$fishbase_valid <- validate_names(manuals$worms_valid)

manuals$fishbase_valid[manuals$worms_valid == 'Acanthurus nigros'] <- 'Acanthurus nigroris'
manuals$fishbase_valid[manuals$worms_valid == 'Bodianus pulcher'] <- 'Semicossyphus pulcher'
manuals$fishbase_valid[manuals$worms_valid == 'Compsanthias hawaiiensis'] <- 'Pseudanthias hawaiiensis'
manuals$fishbase_valid[manuals$worms_valid == 'Compsanthias ventralis'] <- 'Pseudanthias ventralis'
manuals$fishbase_valid[manuals$worms_valid == 'Holocanthus townsendi'] <- 'Holacanthus bermudensis'
manuals$fishbase_valid[manuals$worms_valid == 'Pomacentrus maafu'] <- 'Pomacentrus moluccensis'
manuals$fishbase_valid[manuals$worms_valid == 'Hypoplectrus hybrid'] <- 'Hypoplectrus sp'
manuals$fishbase_valid[str_which(manuals$worms_valid, '\\scf')] <- 'Eviota sp'

fb_valid <- fb_valid %>% filter(!is.na(SpecCode))

fb_valid <- bind_rows(fb_valid, manuals) %>% arrange(worms_valid)
fb_valid <- left_join(fb_valid, fb_sp[1:2], by = "SpecCode")
fb_valid <- fb_valid %>% select(!SpecCode) %>% rename(fishbase_valid = Species) %>%
  bind_rows(manuals)

allsp2 <- left_join(allsp, fb_valid, by = 'worms_valid')

# ==============================================================================
# FILL IN VALID NAME FOR NON-SPECIES RESOLUTION
# ==============================================================================

allsp2 %>% filter(is.na(taxon_resolution)) %>% View

allsp2$Species[is.na(allsp2$taxon_resolution)] <- 'sp'
allsp2$taxon_resolution[is.na(allsp2$taxon_resolution)] <- 'genus'

allsp2 %>% filter(taxon_resolution != 'species') %>% 
  select(taxon_resolution, genus_sp, worms_valid, genus_valid, family_valid, fishbase_valid) %>%
  distinct %>% View

allsp2$worms_valid[which(is.na(allsp2$worms_valid) & allsp2$taxon_resolution %in% c('genus', 'genus specified'))] <- with(allsp2[which(is.na(allsp2$worms_valid) & allsp2$taxon_resolution %in% c('genus', 'genus specified')), ], paste(genus_valid, Species, sep = ' '))

allsp2$worms_valid[which(is.na(allsp2$worms_valid) & allsp2$taxon_resolution %in% c('family', 'lowres'))] <- allsp2$family_valid[which(is.na(allsp2$worms_valid) & allsp2$taxon_resolution %in% c('family', 'lowres'))]

allsp2$worms_valid[which(is.na(allsp2$worms_valid) & allsp2$taxon_resolution == 'family specified')] <- with(allsp2[which(is.na(allsp2$worms_valid) & allsp2$taxon_resolution == 'family specified'), ], paste(family_valid, Species, sep = ' '))

# fishbase trait name
allsp2$fishbase_valid[which(is.na(allsp2$fishbase_valid) & allsp2$taxon_resolution %in% c('genus', 'genus specified'))] <- with(allsp2[which(is.na(allsp2$fishbase_valid) & allsp2$taxon_resolution %in% c('genus', 'genus specified')), ], paste(genus_valid, 'sp', sep = ' '))

allsp2$fishbase_valid[which(is.na(allsp2$fishbase_valid) & allsp2$taxon_resolution %in% c('family', 'family specified', 'lowres'))] <- allsp2$family_valid[which(is.na(allsp2$fishbase_valid) & allsp2$taxon_resolution %in% c('family', 'family specified', 'lowres'))]

allsp2 %>% filter(is.na(worms_valid))
allsp2 %>% filter(is.na(fishbase_valid))
summary(allsp2)
allsp2$fishbase_valid[allsp2$genus_valid == 'Menaphorus'] <- NA
allsp2 %>% filter(is.na(genus_valid) | is.na(family_valid)) %>% View

write_csv(allsp2, 'dt_species.csv')

# ==============================================================================
# APPLY CLEAN TAXONOMY TO DATA
# ==============================================================================

dt_valid <- dt_valid[-c(5:7,19)]
colnames(dt_valid)[16:17] <- c('Family', 'Genus')
dt_valid$Species <- word(dt_valid$worms_valid, 2)

fix <- dt_valid %>% filter(is.na(taxaID)) %>% distinct(Family, Genus, Species) %>%
  mutate(genus_sp = paste(Genus, Species, sep = ' '))
fix$taxon_resolution <- 'species'
fix$taxon_resolution[fix$Species == 'sp'] <- 'genus'
fix$worms_valid <- NA
fix$worms_valid[fix$taxon_resolution == 'species'] <- wm_records_names(name = fix$genus_sp[fix$taxon_resolution == 'species']) %>% 
  bind_rows %>% pull(valid_name)
fix$genus_valid <- word(fix$worms_valid, 1)
fix$genus_valid[is.na(fix$worms_valid)] <- fix$Genus[is.na(fix$worms_valid)]
fix$worms_valid[is.na(fix$worms_valid)] <- with(fix[which(is.na(fix$worms_valid)),], paste(genus_valid, Species, sep = ' '))
fix$fishbase_valid <- validate_names(fix$worms_valid)
fix$fishbase_valid[is.na(fix$fishbase_valid)] <- fix$worms_valid[is.na(fix$fishbase_valid)]
fix$taxaID <- 4837:(4837+nrow(fix)-1)
allsp2 <- bind_rows(allsp2, fix)

# give valid taxaIDs
dt_valid <- allsp2 %>% select(taxaID, Family, Genus, Species) %>%
  distinct(Family, Genus, Species, .keep_all = T) %>%
  left_join(dt_valid, ., by = c('Family', 'Genus', 'Species'))

# clean up column names
dt_cleanfish <- dt_notclean %>% select(!c(Family, Genus, Species, taxon_resolution)) %>% 
  select(!ends_with('valid'))
# merge full dataset
dt_cleanfish <- dt_valid %>% select(!c(Family, Genus, Species)) %>%
  bind_rows(dt_cleanfish, .)
str(dt_cleanfish)
dt_cleanfish <- allsp2 %>% select(taxaID, taxon_resolution, worms_valid, fishbase_valid) %>%
  left_join(dt_cleanfish, ., by = 'taxaID') # join in clean taxa
dt_cleanfish <- dt_cleanfish %>% filter(!taxaID %in% allsp2$taxaID == F) # omits

summary(dt_cleanfish)
dt_cleanfish %>% filter(str_detect(taxon_resolution, 'specified')) %>% View

write_csv(dt_cleanfish, 'macro_reef_fish_clean.csv')


# ==============================================================================
# CLEAN METADATA
# ==============================================================================

sort(unique(dt_assemblage$DataID)) == sort(unique(dt_meta$DataID)) # something went wrong at 51
tibble(data = sort(unique(dt_assemblage$DataID))[51:88], 
       meta = sort(unique(dt_meta$DataID))[51:88]) %>% View