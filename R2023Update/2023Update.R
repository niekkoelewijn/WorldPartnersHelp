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


### Get coordinates of villages of households or farmers that received food aid


## Households

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
# plot(UniqueVillagesFoodSF["state"], add = T, label = UniqueVillagesFoodSF["Village/town"])
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


## Farmers

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


### Join coordinate with data


## Households

# Join food dataset with unique villages with coordinates
VillageDataHouseholds <- FoodAidData %>% 
  
  # Select columns of importance
  dplyr::select(Date, "Issuer Organization", "Family name", "Village/town", "Oblast",
                "Number of people in household", "Of which children (< 18 years)",
                "The benficiary receives: /Food package", "The benficiary receives: /Seed package") %>% 
  
  # Rename Oblast to state
  dplyr::rename(state = Oblast) %>% 
  
  # Join with the table with coordinates for each unique village
  inner_join(UniqueVillagesFood, by = c("Village/town", "state"))

# Rename issuer organizations
VillageDataHouseholds$`Issuer Organization`[which(VillageDataHouseholds$`Issuer Organization` == "TEAM SOUTH (TCI)")] = "Team South"
VillageDataHouseholds$`Issuer Organization`[which(VillageDataHouseholds$`Issuer Organization` == "TEAM EAST")] = "Team East"

# Replace villages without coordinate with "others"
VillageDataHouseholds = VillageDataHouseholds %>% 
  mutate(`Village/town` = if_else(`Issuer Organization` == "Team South" & is.na(lat), "Others, Team South", `Village/town`)) %>% 
  mutate(`Village/town` = if_else(`Issuer Organization` == "Team East" & is.na(lat), "Others, Team East", `Village/town`)) %>% 
  mutate(lat = if_else(is.na(lat) & `Issuer Organization` == "Team South", 46.645240, lat)) %>% 
  mutate(lng = if_else(is.na(lng) & `Issuer Organization` == "Team South", 32.626960, lng)) %>% 
  mutate(lat = if_else(is.na(lat) & `Issuer Organization` == "Team East", 48.743296, lat)) %>% 
  mutate(lng = if_else(is.na(lng) & `Issuer Organization` == "Team East", 37.590617, lng))
  

## Replace outliers with coordinate of distribution point

# Create a lat and a lng column
UniqueVillagesFoodCoordinates <- UniqueVillagesFoodSF %>% 
  dplyr::mutate(lng = st_coordinates(UniqueVillagesFoodSF)[,1],
                lat = st_coordinates(UniqueVillagesFoodSF)[,2])

# Get outliers by selecting villages outside normal range
UniqueVillagesFoodCoordinates[which(UniqueVillagesFoodCoordinates$lat > 48.961177 &  UniqueVillagesFoodCoordinates$lng < 33.013199),] %>%

  #Village/town
  dplyr::mutate(VillageNameLatin = stri_trans_general(str = `Village/town`, id = "Ukrainian-Latin/BGN"))

# Replace coordinates of outliers to coordinates of distribution point
VillageDataHouseholds = VillageDataHouseholds %>% 
  mutate(`Village/town` = if_else(`Village/town` == "Киселівка" & state == "Ми", "Others, Team South", `Village/town`)) %>% 
  mutate(`Village/town` = if_else(`Village/town` == "Запоріжжя" & state == "З", "Others, Team East", `Village/town`)) %>% 
  mutate(`Village/town` = if_else(`Village/town` == "Миколаїв" & state == "Миколаїв", "Others, Team South", `Village/town`)) %>% 
  mutate(`Village/town` = if_else(`Village/town` == "Михайлівка" & state == "Ми", "Others, Team South", `Village/town`)) %>% 
  mutate(lat = if_else(`Village/town` == "Others, Team South", 46.645240, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Others, Team South", 32.626960, lng)) %>% 
  mutate(lat = if_else(`Village/town` == "Others, Team East", 48.743296, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Others, Team East", 37.590617, lng)) 

# # Visualise VillageDataHouseholds
# VillageDataHouseholdsSf <- st_as_sf(VillageDataHouseholds, coords = c("lng", "lat"), crs = 4326)
# 
# plot(st_geometry(UkraineSf))
# plot(VillageDataHouseholdsSf["Issuer Organization"], add = T)


## Farmers

# Join seed datasete with unique villages with coordinates
VillageDataFarmers <- SeedAidData %>% 

  # Join with the table with coordinates for each unique village
  inner_join(UniqueVillagesSeed, by = c("City/Village"))
