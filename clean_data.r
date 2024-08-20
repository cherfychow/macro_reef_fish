

require(dplyr)
require(readr)
require(taxize)
require(rfishbase)
require(worrms)
require(stringr)
require(purrr)

# load in data
dt_assemblage <- read_csv('src/macro_reef_fish_obs.csv')
gc() # keeping memory usage low

summary(dt_assemblage)

# TAXONOMY CLEAN ===================================================================================

# dt_assemblage %>% distinct(Family, Genus, Species) %>% arrange(Family, Genus, Species) %>% View

# clean epithets
dt_assemblage$Species[with(dt_assemblage, which(Genus == 'Eviota' & str_detect(Species, '^cf')))] <- str_replace(dt_assemblage$Species[with(dt_assemblage, which(Genus == 'Eviota' & str_detect(Species, '^cf')))], 'cf', 'cf. ')
dt_assemblage <- dt_assemblage[-which(dt_assemblage$Genus == 'Blennioidei'),]
# remove genera, but allow the cf. records
dt_assemblage$Genus[which(str_detect(dt_assemblage$Species, '\\s') & str_detect(dt_assemblage$Species, '^cf') == F)] <- word(dt_assemblage$Species[which(str_detect(dt_assemblage$Species, '\\s') & str_detect(dt_assemblage$Species, '^cf') == F)], 1)
dt_assemblage$Species[str_which(dt_assemblage$Species, '^cf', negate = T)] <- str_remove(dt_assemblage$Species[str_which(dt_assemblage$Species, '^cf', negate = T)], '^[A-Z]{1}[a-z]+\\s')
dt_assemblage$Species[str_which(dt_assemblage$Species, '^cf', negate = T)] <- word(dt_assemblage$Species[str_which(dt_assemblage$Species, '^cf', negate = T)], 1) # just epithets, no subspecies
dt_assemblage$Species[str_which(dt_assemblage$Genus, '\\s')] <- dt_assemblage$Genus[str_which(dt_assemblage$Genus, '\\s')] %>%
  str_extract(., '(?<=\\s)[A-Za-z]+$') %>% str_to_lower
dt_assemblage$Genus[str_which(dt_assemblage$Genus, '\\s')] <- dt_assemblage$Genus[str_which(dt_assemblage$Genus, '\\s')] %>%
  str_extract(., '^[A-Za-z]+(?=\\s)')
dt_assemblage$Species[with(dt_assemblage, is.na(Genus) == F & is.na(Species))] <- 'sp'
dt_assemblage$Genus[dt_assemblage$Genus == 'Sp'] <- NA
dt_assemblage$Genus[dt_assemblage$Genus == 'Labrisomid'] <- NA
dt_assemblage$Family[dt_assemblage$Genus == 'Squirrelfish'] <- 'Holocentridae'
dt_assemblage$Genus[dt_assemblage$Genus == 'Squirrelfish'] <- NA

# uncertains
# dt_assemblage %>% distinct(Genus, Species) %>% filter(str_detect(Species, '^sp.{1,2}$')) %>% View
dt_assemblage$Species[dt_assemblage$Species %in% c('sp.', 'spp', 'species', 'spies')] <- 'sp'
dt_assemblage$Species[dt_assemblage$Species == 'spx'] <- 'sp1'
dt_assemblage$Species[dt_assemblage$Species == 'spz'] <- 'sp2'
dt_assemblage$Species[dt_assemblage$Species == 'spd'] <- 'sp1'
dt_assemblage$Family[dt_assemblage$Genus == 'Blenny'] <- 'Blenniidae'
dt_assemblage$Species[dt_assemblage$Genus == 'Blenny'] <- NA
dt_assemblage$Genus[dt_assemblage$Genus == 'Blenny'] <- NA
dt_assemblage$Species <- str_replace(dt_assemblage$Species, 'i{3}$', 'ii')
dt_assemblage$Species[which(dt_assemblage$Species == 'Red')] <- 'sp1'

# parrotfish specific
dt_assemblage$Species[dt_assemblage$Genus == 'Scarinissp8'] <- 'sp8'
dt_assemblage$Genus[dt_assemblage$Genus == 'Scarinissp8'] <- 'Scarini'
dt_assemblage$Species[str_which(dt_assemblage$Species, '^\\d$')] <- paste0('sp', dt_assemblage$Species[str_which(dt_assemblage$Species, '^\\d$')])
dt_assemblage$Genus[dt_assemblage$Family == 'Scaridae' & is.na(dt_assemblage$Genus)] <- 'Scarini'
dt_assemblage$Family[dt_assemblage$Family == 'Scaridae'] <- 'Labridae'
dt_assemblage$Genus <- str_to_title(dt_assemblage$Genus)
dt_assemblage$Species <- str_to_lower(dt_assemblage$Species)
dt_assemblage$Genus[dt_assemblage$Genus == 'Labrid'] <- NA

# omits
dt_assemblage <- dt_assemblage[-which(str_detect(dt_assemblage$Species, 'crcp|franc') & is.na(dt_assemblage$Genus)),]
dt_assemblage <- dt_assemblage[-which(dt_assemblage$Genus == 'Teleostei'),]


# make a column to help with sorting uncertain records
dt_assemblage$taxon_resolution <- NA
# do the normals first
dt_assemblage$taxon_resolution[with(dt_assemblage, which(is.na(Family) == F & is.na(Genus)))] <- 'family'
dt_assemblage$taxon_resolution[with(dt_assemblage, which(is.na(Genus) == F & is.na(Species) == F))] <- 'species'
dt_assemblage$taxon_resolution[with(dt_assemblage, which(is.na(Genus) == F & str_detect(Species, '^sp$')))] <- 'genus'

# now the exceptions
dt_assemblage$taxon_resolution[with(dt_assemblage, which(Genus == 'Scarini'))] <- 'family'
dt_assemblage$taxon_resolution[with(dt_assemblage, which(is.na(Genus) & str_detect(Species, '^sp\\d{1,2}$')))] <- 'family specified'
dt_assemblage$taxon_resolution[with(dt_assemblage, which(is.na(Genus) == F & str_detect(Species, '^sp\\d{1,2}$')))] <- 'genus specified'
# Scarini special
dt_assemblage$taxon_resolution[with(dt_assemblage, which(Genus == 'Scarini' & str_detect(Species, '^sp\\d{1,2}$')))] <- 'family specified'
dt_assemblage <- dt_assemblage[-which(is.na(dt_assemblage$Family) & is.na(dt_assemblage$Genus)),]

# reorg
dt_assemblage <- relocate(dt_assemblage, taxon_resolution, .after = Species)

# family level consistency, specified from unspecified
dt_assemblage$Species[which(dt_assemblage$taxon_resolution == 'family')] <- NA
dt_assemblage$Family <- word(dt_assemblage$Family, 1) |> str_to_title()
dt_assemblage$Family[dt_assemblage$Family == 'Scarini-Labridae'] <- 'Labridae'

## Automate misspelling fix========================================================================

# generate combined taxonomy
dt_assemblage <- dt_assemblage %>% mutate(genus_sp = case_when(
  taxon_resolution == 'family' ~ Family,
  taxon_resolution == 'family specified' ~ paste(Family, Species, sep = ' '),
  .default = paste(Genus, Species, sep = ' ')
)) %>% relocate(genus_sp, .before = Family)

dt_assemblage %>% distinct(Family, Genus, Species, genus_sp) %>% arrange(genus_sp) %>%  View

# generate species list for checking
sp <- dt_assemblage %>% # only certain, species level IDs for this stage
  filter(taxon_resolution == 'species' & str_detect(Species, '\\.') == F) %>% 
  distinct(genus_sp) %>% pull %>% sort

# fetch taxonomic IDs from fuzzy matching
dt_spcheck <- tibble(genus_sp = sp)
# GBIF is smarter about misspellings than WoRMS, but requires more user input
dt_spcheck$worms_id <- get_wormsid(dt_spcheck$genus_sp, searchtype = 'scientific', fuzzy = T,
                                   marine_only = T, messages = F)
dt_spcheck$gbif_id <- NA
dt_spcheck$gbif_id[is.na(dt_spcheck$worms_id)] <- get_gbifid(dt_spcheck$genus_sp[is.na(dt_spcheck$worms_id)], 
                                                             messages = F, method = 'backbone')

# first round of catching unaccepted names
dt_spcheck %>% filter(is.na(worms_id) & is.na(gbif_id)) # ok only three to fix manually

# get names, identify valid vs synonyms
dt_spcheck <- dt_spcheck %>%
  mutate(across(ends_with('id'), as.numeric))
worms <- wm_record_(dt_spcheck$worms_id |> na.omit())  %>% bind_rows # will take a loong time
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

# gbif is really a middleman for better fuzzy matching misspellings
# put them back through worms

worms2 <- wm_records_names(dt_spcheck$gbif_name |> na.omit(), marine_only = T) %>% bind_rows
# isolate to then rejoin
gbif_match <- dt_spcheck %>% filter(is.na(gbif_id) == F) %>% select(genus_sp, gbif_id, gbif_name)
# remove bad gbif matches (not species level)
gbif_match <- gbif_match %>% filter(str_detect(gbif_name, '\\s'))
gbif_match <- worms2 %>% select(scientificname, status, valid_name, class, family, genus) %>% 
  rename(worms_valid = valid_name, worms_status = status, worms_name = scientificname) %>% 
  distinct %>% left_join(gbif_match, ., by = c('gbif_name' = 'worms_name'))

# rejoin
dt_spcheck <- dt_spcheck %>% filter(!genus_sp %in% gbif_match$genus_sp) %>%
  bind_rows(., gbif_match) %>% arrange(genus_sp)
dt_spcheck <- dt_spcheck[-293,] # remove a duplicated record

dt_spcheck <- dt_spcheck %>% select(!starts_with('gbif'))
dt_spcheck %>% filter(is.na(worms_valid)) %>% View
rm(dt_spcheck2)

# manual validation for these
dt_spcheck <- dt_spcheck %>% 
  mutate(worms_valid = case_when(.default = worms_valid,
                                 genus_sp == "Acanthochromis japonicus" ~ 'Acanthochromis polyacanthus',
                                 genus_sp == "Bipartitus bipartitus" ~ "Macropharyngodon bipartitus",
                                 genus_sp == "Calostomus coralinus" ~ 'Calotomus carolinus',
                                 genus_sp == "Cantherhines maculatus" ~ 'Cantherhines maculata',
                                 genus_sp == "Chromis tricincta" ~ 'Chrysiptera tricincta',
                                 genus_sp == "Ecsenius flavus" ~ 'Plagiotremus flavus',
                                 genus_sp == "Enneanectes tutuilae" ~ 'Enneapterygius tutuilae',
                                 genus_sp == "Epinephelides guttatus" ~ 'Epinephelus guttatus',
                                 genus_sp == "Holocanthus townsendi" ~ 'Holacanthus townsendi', # hybrid
                                 genus_sp == "Hypoplectrus tann" ~ 'Hypoplectrus randallorum',
                                 genus_sp == "Labroides manabei" ~ 'Labropsis manabei',
                                 genus_sp == "Pomacentrus margaritifer" ~ 'Pycnochromis margaritifer',
                                 genus_sp == "Sargus sargus" ~ 'Diplodus sargus',
                                 genus_sp == "Ucla macrospilus" ~ 'Acanthemblemaria macrospilus'
                                 ))
dt_spcheck$worms_valid[is.na(dt_spcheck$worms_valid)] <- dt_spcheck$genus_sp[is.na(dt_spcheck$worms_valid)]
# double check
rm(sp, worms, worms2, notfish, gbif, gbif_match)

## IMPLEMENT FIXES =================================================================================

dt_cleanfish <- dt_spcheck %>% select(genus_sp, worms_valid) %>% 
  left_join(dt_assemblage, ., by = "genus_sp")

dt_cleanfish %>% filter(taxon_resolution != 'species') %>% 
  distinct(Family, Genus, Species, genus_sp, taxon_resolution) %>% arrange(genus_sp) %>% View

# eliminate non-fish
notfish <- dt_spcheck %>% 
  filter(is.na(class) == F) %>% 
  filter(!class %in% c('Teleostei', 'Elasmobranchii')) %>%
  filter(!family == 'Elapidae') %>% 
  pull(genus_sp)

dt_cleanfish <- dt_cleanfish %>% filter(!genus_sp %in% notfish)
notfish <- word(notfish, 1) # also use the genera to filter it out
dt_cleanfish <- dt_cleanfish %>% filter(!Genus %in% notfish)
dt_cleanfish <- dt_cleanfish[-which(dt_cleanfish$Genus == 'Ornatus'),]
dt_cleanfish <- dt_cleanfish[-which(dt_cleanfish$Genus == 'Chelonia'),]

## VALIDATE MATCH GENUS/FAMILY =====================================================================

# can't reuse species level validation 
# because synonymised species can get mixed up with misspelled genera
dt_cleanfish <- dt_cleanfish %>% 
  mutate(genus_input = case_when(
    is.na(worms_valid) == F ~ word(worms_valid, 1),.default = Genus))

genus <- dt_cleanfish %>% distinct(Genus) %>% filter(is.na(Genus) == F) %>% arrange(Genus)
 
genus$worms_id <- get_wormsid(genus$Genus, marine_only = T, messages = F, accepted = T,
                              searchtype = 'scientific', fuzzy = T) %>% as.numeric
# manual matches
genus$worms_id[103] <- 159492
genus$worms_id[156] <- 203854
genus$worms_id[237] <- 204365
genus$worms_id[378] <- 205521
genus$worms_id[412] <-159383

genus <- wm_record_(genus$worms_id |> na.omit()) %>% 
  bind_rows %>% left_join(genus, ., by = c('worms_id' = 'AphiaID'))
genus <- genus %>% select(Genus, status, family, genus, worms_id) %>% distinct %>% 
  rename(genus_input = Genus, worms_status = status, family_valid = family, genus_valid = genus)

# some manual fixes
genus %>% filter(is.na(genus_valid))
genus$family_valid[
  genus$genus_input %in% c('Scarini', 'Scarideo')] <- 'Labridae'
genus$genus_valid[
  genus$genus_input %in% c('Scarini', 'Scarideo')] <- 'Scarini'
genus$family_valid[genus$genus_input %in% c('Menephorus', 'Menaphorus')] <- 'Serranidae'
genus$genus_valid[genus$genus_input %in% c('Menephorus', 'Menaphorus')] <- 'Menephorus'

# merge in
dt_cleanfish <- left_join(dt_cleanfish, genus[c(1,3,4)], by = "genus_input")

# fill in families
# dt_cleanfish %>% filter(taxon_resolution == 'family') %>% distinct(Family) %>% arrange(Family) %>% View
dt_cleanfish$family_valid[is.na(dt_cleanfish$family_valid)] <- dt_cleanfish$Family[is.na(dt_cleanfish$family_valid)]
dt_cleanfish$family_valid[with(dt_cleanfish, which(taxon_resolution == 'family' & Family == 'Caesioinidae'))] <- 'Caesionidae'

# fill in worms_valid for uncertains
dt_cleanfish$worms_valid[
  with(dt_cleanfish, 
       which(is.na(worms_valid) & str_detect(taxon_resolution, 'genus'))
  )] <- with(dt_cleanfish[with(dt_cleanfish, 
                               which(is.na(worms_valid) & str_detect(taxon_resolution, 'genus'))
  ),], paste(genus_valid, 'sp', sep = ' '))
dt_cleanfish$worms_valid[with(dt_cleanfish, which(is.na(worms_valid) & taxon_resolution == 'family specified'))] <- with(dt_cleanfish[with(dt_cleanfish, which(is.na(worms_valid) & taxon_resolution == 'family specified')),], paste(family_valid, Species, sep = ' '))
dt_cleanfish$worms_valid[with(dt_cleanfish, which(is.na(worms_valid) & taxon_resolution == 'family'))] <- dt_cleanfish$family_valid[with(dt_cleanfish, which(is.na(worms_valid) & taxon_resolution == 'family'))]

# dt_cleanfish %>% filter(is.na(worms_valid)) %>% View
# keep the ones left over as is
dt_cleanfish$worms_valid[is.na(dt_cleanfish$worms_valid)] <- dt_cleanfish$genus_sp[is.na(dt_cleanfish$worms_valid)]

dt_cleanfish %>% select(ends_with('valid')) %>% distinct %>% 
  arrange(family_valid, worms_valid) %>%View
dt_cleanfish %>% select(ends_with('valid')) %>% distinct %>% 
  filter(is.na(genus_valid)) %>% arrange(family_valid, worms_valid) %>% View
dt_cleanfish$genus_valid[with(dt_cleanfish, which(taxon_resolution %in% c('genus specified', 'species', 'genus') & is.na(genus_valid)))] <- dt_cleanfish$worms_valid[with(dt_cleanfish, which(taxon_resolution %in% c('genus specified', 'species', 'genus') & is.na(genus_valid)))] %>% word(., 1)
# parrotfish special
dt_cleanfish$family_valid[dt_cleanfish$family_valid == 'Scaridae'] <- 'Labridae'

# FINAL REORG
dt_cleanfish <- dt_cleanfish %>% select(!c(genus_sp:Species, genus_input)) %>% 
  relocate(worms_valid:genus_valid, .before = taxon_resolution)

## WoRMS and FISHBASE HARMONISE=====================================================================

# needed for trait compilation

fb_valid <- dt_cleanfish %>% filter(taxon_resolution == 'species') %>% 
  distinct(worms_valid) %>% arrange(worms_valid)
fb_sp <- species(species_list = fb_valid$worms_valid) # get fishbase valid species
fb_syn <- synonyms(species_list = fb_valid$worms_valid)
fb_valid <- left_join(fb_valid, fb_sp[1:2], by = c('worms_valid' = 'Species'))

# isolate species that can't match
manuals <- fb_valid %>% filter(is.na(SpecCode)) %>% select(!SpecCode) %>% 
  left_join(., fb_syn[c(1,3)], by = c("worms_valid" = "synonym"))
fb_valid <- fb_valid %>% filter(!is.na(SpecCode))

dt_cleanfish$genus_valid[dt_cleanfish$worms_valid == 'Cantherhines maculata'] <- 'Canthidermis'
dt_cleanfish$worms_valid[dt_cleanfish$worms_valid == 'Cantherhines maculata'] <- 'Canthidermis maculata'
fb_valid <- bind_rows(fb_valid, manuals) %>% arrange(worms_valid)

fb_valid <- left_join(fb_valid, fb_sp[1:2], by = "SpecCode")
fb_valid %>% select(!SpecCode) %>% rename(fishbase_valid = Species) %>% 
  left_join(dt_cleanfish, ., by = "worms_valid") %>% relocate(fishbase_valid, .after = worms_valid) %>% View

write_csv(dt_cleanfish, 'src/macro_reef_fish_clean.csv')

summary(dt_cleanfish)
