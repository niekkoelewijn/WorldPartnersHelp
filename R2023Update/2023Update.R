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



### Join coordinate with data


## Households

# Find villages/towns that are situated on illogical places
UniqueVillagesFood[which(UniqueVillagesFood$lat > 48.961177 &  UniqueVillagesFood$lng < 33.013199),]

# Correct the 4 places that are situated on illogical places
UniqueVillagesFoodCorrected <- UniqueVillagesFood %>% 
  
  # Mutate lat and lng of the 4 places
  dplyr::mutate(lat = if_else(`Village/town` == "Киселівка" & state == "Ми", 46.645240, lat)) %>% 
  dplyr::mutate(lat = if_else(`Village/town` == "Запоріжжя" & state == "З", 48.743296, lat)) %>% 
  dplyr::mutate(lat = if_else(`Village/town` == "Миколаїв" & state == "Миколаїв", 46.645240, lat)) %>% 
  dplyr::mutate(lat = if_else(`Village/town` == "Михайлівка" & state == "Ми", 46.645240, lat)) %>% 
  dplyr::mutate(lng = if_else(`Village/town` == "Киселівка" & state == "Ми", 32.626960, lng)) %>% 
  dplyr::mutate(lng = if_else(`Village/town` == "Запоріжжя" & state == "З", 37.590617, lng)) %>% 
  dplyr::mutate(lng = if_else(`Village/town` == "Миколаїв" & state == "Миколаїв", 32.626960, lng)) %>% 
  dplyr::mutate(lng = if_else(`Village/town` == "Михайлівка" & state == "Ми", 32.626960, lng))
  
# Check if all villages/towns are on logical places
UniqueVillagesFoodCorrected[which(UniqueVillagesFoodCorrected$lat > 48.961177 &  UniqueVillagesFoodCorrected$lng < 33.013199),]

# Join food dataset with unique villages with coordinates
VillageDataHouseholds <- FoodAidData %>% 
  
  # Select columns of importance
  dplyr::select(Date, "Issuer Organization", "Family name", "Village/town", "Oblast",
                "Number of people in household", "Of which children (< 18 years)",
                "The benficiary receives: /Food package", "The benficiary receives: /Seed package") %>% 
  
  # Rename Oblast to state
  dplyr::rename(state = Oblast) %>% 
  
  # Join with the table with coordinates for each unique village
  inner_join(UniqueVillagesFoodCorrected, by = c("Village/town", "state"))

# Check if all villages/ towns are on logical places
VillageDataHouseholds[which(VillageDataHouseholds$lat > 48.961177 &  VillageDataHouseholds$lng < 33.013199),]

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

# Determine total per city
TotalVillageHouseholds <- VillageDataHouseholds %>% 
  
  # Group by city and state
  group_by(`Village/town`, state) %>% 
  
  # Summarise totals per village
  summarise(`Issuer Organization` = first(`Issuer Organization`),
            `Total families` = n_distinct(`Family name`),
            `Total persons` = sum(`Number of people in household`),
            `Total children` = sum(`Of which children (< 18 years)`),
            `Total food packages` = sum(`The benficiary receives: /Food package`),
            `Total seed packages` = sum(`The benficiary receives: /Seed package`)) %>% 
  
  # Join with unique villages for coordinates
  inner_join(UniqueVillagesFoodCorrected, by = c("Village/town","state" ))

# Check if there are no illogical places in the TotalVillageHouseholds dataset
TotalVillageHouseholds[which(TotalVillageHouseholds$lat > 48.961177 &  TotalVillageHouseholds$lng < 33.013199),]

# Make totals spatial
TotalPerVillage <- st_as_sf(TotalVillageHouseholds, coords = c("lng", "lat"), crs = st_crs(4326))  

# Add coordinates
TotalPerVillage$x <- st_coordinates(TotalPerVillage)[,1]
TotalPerVillage$y <- st_coordinates(TotalPerVillage)[,2]

# Add latin city name as column
TotalPerVillage <- TotalPerVillage %>% 
  
  # Ukrainian cyrillic to latin
  mutate(VillageNameLatin = stri_trans_general(str = `Village/town`, id = "Ukrainian-Latin/BGN"))


## Get data per Issuer Organization

# Summarize all data per Issuer Organization
IssuerOrganizationSummary <- TotalPerVillage %>% 
  
  # Group by city
  group_by(`Issuer Organization`) %>% 
  
  # Summarise totals per village
  summarise(`Total families` = sum(`Total families`),
            `Total persons` = sum(`Total persons`),
            `Total children` = sum(`Total children`),
            `Total food packages` = sum(`Total food packages`),
            `Total seed packages` = sum(`Total seed packages`)) %>% 
  
  # Remove redundant geometry column
  tibble() %>% 
  dplyr::select(-geometry)

# Add lng and lat column to IssuerOrganizationSummary
IssuerOrganizationSummary$x <- c(37.590617, 32.626960) 
IssuerOrganizationSummary$y <- c(48.743296, 46.645240)

# # Visualise TotalPerVillage
# plot(st_geometry(UkraineSf))
# plot(TotalPerVillage["Issuer Organization"], add = T)


## Farmers

# Join seed datasete with unique villages with coordinates
VillageDataFarmers <- SeedAidData %>% 

  # Join with the table with coordinates for each unique village
  inner_join(UniqueVillagesSeed, by = c("City/Village"))

# Latinise farmer villages and oblasts
VillageDataFarmers <- VillageDataFarmers %>% 
  
    # City/Village
    dplyr::mutate(VillageNameLatin = stri_trans_general(str = `City/Village`, id = "Ukrainian-Latin/BGN")) %>%

    # State
    dplyr::mutate(OblastNameLatin = stri_trans_general(str = state, id = "Ukrainian-Latin/BGN"))

# Find coordinate for farmers with only the city name, not oblast
NoCoordinateFarmers <- VillageDataFarmers %>% 
  
  # Filter na's
  dplyr::filter(is.na(lat)) %>% 
  
  # Get lat and lon of village by its name
  geocode(city = `City/Village`, country = country, 
          method = "osm", lat = lat, long = lng )  %>% 
  
  # Select columns to create the same order as VillageDataFarmers
  dplyr::select(Oblast, `City/Village`, Address, state, country,
                lat...10, lng...11, VillageNameLatin, OblastNameLatin) %>% 
  
  # Rename lat and lng
  dplyr::rename(lat = lat...10, lng = lng...11)

# Combine VillageDataFarmers and NoCoordinateFarmers
VillageDataFarmersComplete <- VillageDataFarmers %>% 
  
  # Select only villages with coordinate
  dplyr::filter(!is.na(lat)) %>% 
  
  # Combine the rows of the two datasets
  dplyr::bind_rows(NoCoordinateFarmers)

# Get farmer list without na's
VillageDataFarmersNoNA<- VillageDataFarmersComplete %>% 
  
  # Filter
  dplyr::filter(!is.na(lat))

# Make data spatial
VillageDataFarmersCompleteSf <- VillageDataFarmersComplete %>%

  # Remove missing values for coordinates
  dplyr::filter(!is.na(lat)) %>%

  st_as_sf(coords = c("lng", "lat"), crs = 4326)

# # Create CSV of farmers that still have no coordinate
# VillageDataFarmersComplete %>% 
#   
#   # Filter na's
#   dplyr::filter(is.na(lat)) %>% 
#   
#   # Write csv
#   write_csv(file = paste0(InputDirectory, "FarmersUnkownLocation.csv"))
# 
# # Plot unique villages
# plot(st_geometry(UkraineSf))
# plot(VillageDataFarmersCompleteSf["state"], add = T)


## Write processed data to csv

# Create path
path <- "~/WorldPartnersHelp/Output2023/"

# Write important datafiles to csv
write_csv(TotalPerVillage, file = paste0(path, "TotalPerVillageHouseholds", ".csv"))
write_csv(IssuerOrganizationSummary, file = paste0(path, "IssuerOrganizationHouseholds", ".csv"))
write_csv(VillageDataFarmersNoNA, file = paste0(path, "VillageDataFarmers", ".csv"))


