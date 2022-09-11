# Food aid distributed to Ukrainian towns and villages 
# Commossioned by Worldparners
# Excecuded by Niek Koelewijn
# September 2022

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
if(!"atlastools" %in% rownames(installed.packages())){install.packages("atlastools")}

# Load required packages
library(rgdal)
library(rgeos)
library(sf)
library(tidyverse)
library(lubridate)
library(stringi)
library(tidygeocoder)
library(raster)
library(atlastools)
library(data.table)

# Load the data from the "Data" directory
InputData <- read_csv(file = "~/WorldPartnersHelp/Data/FoodAidData.csv")

# Get unique village names from input data
UniqueVillages <- InputData %>% 
  
  # Select only villages
  dplyr::select(`Village/town`) %>% 
  
  # Group villages
  group_by(`Village/town`) %>% 
  
  # Get distinct villages
  distinct(`Village/town`) %>% 
  
  # Create country column
  mutate(country = "Ukraine") %>% 
  
  # Get lat and lon of village by its name
  geocode(city = `Village/town`, country = country, method = "osm", lat = lat, long = lng )

# Script to prove that there are indeed 999 unique town names  
# InputDataCopy <- InputData
# InputDataCopy <- InputDataCopy %>% 
#   dplyr::select(`Village/town`) %>% 
#   tibble::add_row(`Village/town` = "Spakenburg")

# Select and adapt data in piped structure
VillageData <- InputData %>% 
  
  # Select columns of importance
  dplyr::select(Date, "Issuer Organization", "Family name", "Village/town",
         "Potato for consumption (in kg)?", "Vegetables (in kg)?",
         "Potato seeds (in kg)?", "Plant onions seeds (in kg)?",
         "Vegetables seeds (number of bag of seeds)?",
         "Flower (in kg)?", "Canned fish/meat (number of cans)?",
         "Salt/sugar (in kg)?", "Pasta/rice (in kg)?",
         "Canned baby food or milk powder (number of cans)?",
         "Soup/noodles (number of cans)?", "Other goods?") %>%
  
  # Join with the table with coordinates for each unique village
  inner_join(UniqueVillages, by = "Village/town") %>% 
  
  # Drop cities that do not have a geographic location
  drop_na(lat) %>% 
  
  # Make sf out of the geocoded tibble
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(4326))

# Get shape of Ukraine
UkraineBoundaries <- getData('GADM', country='UKR', level=1)

# Get lng and lat from the point dataset
VillageData$lng <- st_coordinates(VillageData)[,1]
VillageData$lat <- st_coordinates(VillageData)[,2]

# Visualize villages in Ukraine
plot(UkraineBoundaries)
points(x = VillageData$lng, y = VillageData$lat)

# Remove points from Crimea
Sevastopol <- st_as_sf(UkraineBoundaries[which(UkraineBoundaries$NAME_1=="Sevastopol'"),]) 
Crimea <- st_as_sf(UkraineBoundaries[which(UkraineBoundaries$NAME_1=="Crimea"),])
VillageData <- st_as_sf(VillageData)
VillageData <- atl_filter_bounds(VillageData , x = "lng", y = "lat", 
                  sf_polygon = Sevastopol, remove_inside = T)
VillageData <- atl_filter_bounds(VillageData , x = "lng", y = "lat", 
                                 sf_polygon = Crimea, remove_inside = T)


# # Load dataset with Ukrainian villages with lat and lon
# UkraineVillagesDownload <- read_csv("~/WorldPartnersHelp/Data/UA/VillagesLatLon.csv")
# 
# # Select and adapt village in piped structure
# UkraineVillages <- UkraineVillagesDownload %>% 
#   
#   # Select columns of importance
#   select(4,5,6) %>% 
#   
#   # drop NA
#   drop_na() %>% 
#   
#   # Rename columns
#   rename(`Village/town` = "Krasnopartizans'ka,Krasnopartizanskaja,Stancija Krasnopartizans'ka,Stancija Krasnopartizanskaja,Tashli-Dair,Tasli Dayir,Taşlı Dayır,Краснопартизанская,Краснопартизанська,Станция Краснопартизанская,Станція Краснопартизанська,Ташли-Даір",
#          Lat = "45.40145",
#          Lon = "34.21635") %>% 
#   
#   # Join the two datasets on column `Village/town`
#   full_join(VillageData, by = "Village/town") %>% 
#   
#   # Remove empty dates
#   drop_na(Date)

