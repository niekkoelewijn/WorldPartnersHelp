# Food aid distributed to Ukrainian towns and villages 
# Commossioned by Worldparners
# Excecuded by Niek Koelewijn
# September 2023

## Script with the aim to preprocess the delivered data from Worldpartners so
## that it can be used for further analysis.

# Install required packages
if(!"rgdal" %in% rownames(installed.packages())){install.packages("rgdal")}
if(!"rgeos" %in% rownames(installed.packages())){install.packages("rgeos")}
if(!"sf" %in% rownames(installed.packages())){install.packages("sf")}
if(!"tidyverse" %in% rownames(installed.packages())){install.packages("tidyverse")}
if(!"lubridate" %in% rownames(installed.packages())){install.packages("lubridate")}
if(!"stringi" %in% rownames(installed.packages())){install.packages("stringi")}
if(!"tidygeocoder" %in% rownames(installed.packages())){install.packages("tidygeocoder")}
if(!"geodata" %in% rownames(installed.packages())){install.packages("geodata")}

# Load required packages
library(rgdal)
library(rgeos)
library(sf)
library(tidyverse)
library(lubridate)
library(stringi)
library(tidygeocoder)
library(raster)
library(geodata)

# Load the data from the "Data2023Update" directory
InputDirectory <- "~/WorldPartnersHelp/Data2023Update/"
FoodAidData <- read_csv(file = paste0(InputDirectory, "FoodAidData.csv"))
SeedAidData <- read_csv(file = paste0(InputDirectory, "SeedAidData.csv"))

# Get unique village names from food input data
UniqueVillagesFood <- FoodAidData %>% 
  
  # Select only villages
  dplyr::select(`Village/town`, Oblast) %>% 
  
  # Get distinct villages
  dplyr::distinct(`Village/town`, Oblast) %>% 
  
  # Create country column
  dplyr::mutate(country = "Ukraine") %>%
  
  # Rename Oblast column to state
  dplyr::rename(state = Oblast) %>% 
  
  # Get lat and lon of village by its name
  geocode(city = `Village/town`, country = country, state = state,
          method = "osm", lat = lat, long = lng )

# Write UniqueVillagesFood as preliminary result
#write_csv(UniqueVillagesFood, paste0(InputDirectory, "PreliminaryResults/UniqueVillagesFood.csv"))

# ## Visualize unique villages food
# 
# # Make data spatial
# UniqueVillagesFoodSF <- UniqueVillagesFood %>% 
#   
#   # Remove missing values for coordinates
#   dplyr::filter(!is.na(lat)) %>% 
#   
#   st_as_sf(coords = c("lng", "lat"), crs = 4326)
# 
# # Load geometry with borders of Ukraine
# Ukraine <- gadm("UKR", level = 1, path = InputDirectory)
# UkraineSf <- st_as_sf(Ukraine)
# plot(st_geometry(UkraineSf))
# 
# # Plot unique villages
# plot(UniqueVillagesFoodSF["state"], add = T)
# 
# # Translate cyrillic to latin
# UniqueVillagesFoodLatin <- UniqueVillagesFood %>% 
#   
#   # Village/town
#   dplyr::mutate(VillageNameLatin = stri_trans_general(str = `Village/town`, id = "Ukrainian-Latin/BGN")) %>% 
#   
#   # State
#   dplyr::mutate(OblastNameLatin = stri_trans_general(str = state, id = "Ukrainian-Latin/BGN"))
#   

# Get unique village names from seed input data
UniqueVillagesSeed <- SeedAidData %>% 
  
  # Select only villages
  dplyr::select(`City/Village`, Oblast) %>% 
  
  # Get distinct villages
  dplyr::distinct(`City/Village`, Oblast) %>% 
  
  # Create country column
  dplyr::mutate(country = "Ukraine") %>%
  
  # Rename Oblast column to state
  dplyr::rename(state = Oblast) %>% 
  
  # Get lat and lon of village by its name
  geocode(city = `City/Village`, country = country, state = state,
          method = "osm", lat = lat, long = lng )

# # Write UniqueVillagesFood as preliminary result
# write_csv(UniqueVillagesSeed, paste0(InputDirectory, "PreliminaryResults/UniqueVillagesSeed.csv"))

# ## Visualize unique villages seed
# 
# # Make data spatial
# UniqueVillagesSeedSF <- UniqueVillagesSeed %>%
# 
#   # Remove missing values for coordinates
#   dplyr::filter(!is.na(lat)) %>%
# 
#   st_as_sf(coords = c("lng", "lat"), crs = 4326)
# 
# 
# # Plot unique villages
# plot(UniqueVillagesSeedSF["state"], add = T)
# 
# # Translate cyrillic to latin
# UniqueVillagesSeedLatin <- UniqueVillagesSeed %>%
# 
#   # Village/town
#   dplyr::mutate(VillageNameLatin = stri_trans_general(str = `City/Village`, id = "Ukrainian-Latin/BGN")) %>%
# 
#   # State
#   dplyr::mutate(OblastNameLatin = stri_trans_general(str = state, id = "Ukrainian-Latin/BGN"))

