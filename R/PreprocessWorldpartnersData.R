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

# Get sum of food delivered per city
TotalDelivered <- VillageData %>% 
  
  # Make the table a tibble again
  tibble() %>% 
  
  # Group by city
  group_by(`Village/town`) %>% 
  
  # Summarise totals per village
  summarise(`Issuer Organization` = first(`Issuer Organization`),
            `Total families` = n_distinct(`Family name`),
            `Total potato (kg)` = sum(`Potato for consumption (in kg)?`),
            `Vegetables (kg)` = sum(`Vegetables (in kg)?`),
            `Total potato seeds (kg)` = sum(`Potato seeds (in kg)?`),
            `Total plant onions seeds (kg)` = sum(`Plant onions seeds (in kg)?`),
            `Total vegetables seeds (n bags)` = sum(`Vegetables seeds (number of bag of seeds)?`),
            `Total flower (kg)` = sum(`Flower (in kg)?`),
            `Total canned fish/meat (n cans)` = sum(`Canned fish/meat (number of cans)?`),
            `Total salt/sugar (kg)` = sum(`Salt/sugar (in kg)?`),
            `Total pasta/rice (kg)` = sum(`Pasta/rice (in kg)?`),
            `Total baby food/milk powder (n cans)` = sum(`Canned baby food or milk powder (number of cans)?`),
            `Total soup/noodles (n cans)` = sum(`Soup/noodles (number of cans)?`)) %>% 
  
  # Join with unique villages for coordinates
  inner_join(UniqueVillages, by = "Village/town", ) %>% 
  
  # Remove column country
  dplyr::select(-country)

# Make totals spatial
TotalPerCity <- st_as_sf(TotalDelivered, coords = c("lng", "lat"), crs = st_crs(4326))

# Visualize
plot(st_coordinates(TotalPerCity))
text(x = VillageData$lng, y = VillageData$lat, labels= VillageData$`Village/town`)

# Remove cities with implausible locations
TotalPerCity <- TotalPerCity[!(TotalPerCity$`Village/town`=="234" | TotalPerCity$`Village/town`=="3"
                               | TotalPerCity$`Village/town`=="4" | TotalPerCity$`Village/town`=="77"
                               | TotalPerCity$`Village/town`=="98" | TotalPerCity$`Village/town`== "Білокуракине"
                               | TotalPerCity$`Village/town`=="Ганусівка" | TotalPerCity$`Village/town`== "Луганськ"
                               | TotalPerCity$`Village/town`=="Павлівка" | TotalPerCity$`Village/town`=="Демьяновка"
                               | TotalPerCity$`Village/town`=="Матвеевка" | TotalPerCity$`Village/town`=="Новоалександровка"),]

# Add lgn and lat column to TotalPerCity
TotalPerCity$x <- st_coordinates(TotalPerCity)[,1]
TotalPerCity$y <- st_coordinates(TotalPerCity)[,2]

# Add latin city name as column
TotalPerCity <- TotalPerCity %>% 
  
  # Ukrainian cyrillic to latin
  mutate(VillageNameLatin = stri_trans_general(str = `Village/town`, id = "Ukrainian-Latin/BGN"))

# Rename all TCI locations to "TCI"
TotalPerCity$`Issuer Organization`[which(TotalPerCity$`Issuer Organization` == "TCI - Chernivtsi")] = "TCI"
TotalPerCity$`Issuer Organization`[which(TotalPerCity$`Issuer Organization` == "TCI - Ivano-frankivsk")] = "TCI"
TotalPerCity$`Issuer Organization`[which(TotalPerCity$`Issuer Organization` == "TCI - Mykolajiv")] = "TCI"

## Write pre-processed data to csv

# Create path
path <- "~/WorldPartnersHelp/GeocodedData/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Write total per city to csv
write_csv(TotalPerCity, file = paste0(path, "TotalsPerCity", ".csv"))



## Action points
# Lijst steden zonder coordinaat uitdraaien
# Jort omcirkelt verdachte dorpen, die uitdraaien
# Jonathan integratie interactieve kaart op website