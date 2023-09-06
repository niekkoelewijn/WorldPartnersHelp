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
InputData <- read_csv(file = "~/WorldPartnersHelp/Data/FoodAidDataSept.csv")

# Get unique village names from input data
UniqueVillages <- InputData %>% 
  
  # Select only villages
  dplyr::select(`Village/town`) %>% 
  
  # Get distinct villages
  distinct(`Village/town`) %>% 
  
  # Create country column
  mutate(country = "Ukraine") %>% 
  
  # Get lat and lon of village by its name
  geocode(city = `Village/town`, country = country, method = "osm", lat = lat, long = lng )

# Add coordinates of others to unique villages
UniqueVillagesWithOthers <- UniqueVillages %>% 
  mutate(lat = if_else(`Village/town` == "Яковлевка", 49.830881, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Яковлевка", 36.135289, lng)) %>% 
  dplyr::add_row(`Village/town` = 'Others, TCI', lng = 31.9692583, lat = 46.9607107) %>% 
  dplyr::add_row(`Village/town` = 'Others, Pilgrim - Zaporizja', lng = 35.1806849, lat = 47.8511949) %>% 
  dplyr::add_row(`Village/town` = 'Others, FriendsExist! - Dubno', lng = 25.7349241, lat = 50.4221361) %>% 
  dplyr::add_row(`Village/town` = 'Others, New Life Centre - Kyiv', lng = 30.5008256, lat = 50.4502522)

# Adapt lat and lng of some villages based on information:
# Fedorivka (NLC) zou degene bij Kharkiv moeten zijn, nu bij Luhanks
# Hetzelfde voor Kalynove (Zhovtneve is de naam bij Kharkiv)
# Ploska (NLC), ligt nu op de Roemeense grens, hoort degene te zijn in Khmelnytskyi Oblast
# Ivanivtsi (TCI), bij Hongaarde grens, hoort degene te zijn in Ivano-Frankivsk Oblast
# Myrcha (NLC), bij Slowaakse grens, hoort degene te zijn in Kyiv Oblast.
# Polyana (TCI), aan Poolse grens, hoort in Mykolaiv Oblast
# Maksymovychi (NLC), aan Poolse grens, hoort in Kyiv Oblast
# Horodok (NLC), hoort in Zhitomir Oblast
# Sosnivka (NLC), hoort in Kyiv Oblast
# Sokolyvka (Pilgrim), à Kyiv Oblast
# Mar'yanivka (NLC), à Kyiv Oblast
# Bolotna (NLC), moet zijn Vulytsya Bolotna in Kyiv Oblast
# Slavne (Pilgrim), à Zaporizja Oblast
UniqueVillagesAdapted <- UniqueVillagesWithOthers %>% 
  mutate(lat = if_else(`Village/town` == "Федорівка", 50.072263, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Федорівка", 36.685417, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Калинове", 49.432298, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Калинове", 37.500518, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Плоска", 50.495908, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Плоска", 27.026275, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Іванівці", 48.576915, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Іванівці", 24.844207, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Мирча", 50.746550, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Мирча", 29.833289, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Поляна", 47.168188, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Поляна", 32.677716, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Максимовичі", 51.194554, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Максимовичі", 29.614131, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Городок", 49.602957, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Городок", 29.195572, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Соснівка", 50.218682, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Соснівка", 29.834593, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Соколивка", 49.906686, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Соколивка", 30.207397, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Мар'янівка", 50.064184, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Мар'янівка", 30.170248, lng)) %>%
  mutate(lat = if_else(`Village/town` == "Болотна", 50.350235, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Болотна", 31.328292, lng)) %>% 
  mutate(lat = if_else(`Village/town` == "Славне", 47.760471, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Славне", 35.687305, lng))

# Get villages that do not have a coordinate
NoCoordinateVillages <- UniqueVillages %>% 
  
  # Select rows with NA for lat and lgn
  dplyr::filter(is.na(lat))

# Select and adapt data in piped structure
VillageData <- InputData %>% 
  
  # Select columns of importance
  dplyr::select(Date, "Issuer Organization", "Family name", "Village/town",
         "Number of people in household", "Of which children (< 18 years)",
         "Potato for consumption (in kg)?", "Vegetables (in kg)?",
         "Potato seeds (in kg)?", "Plant onions seeds (in kg)?",
         "Vegetables seeds (number of bag of seeds)?",
         "Flower (in kg)?", "Canned fish/meat (number of cans)?",
         "Salt/sugar (in kg)?", "Pasta/rice (in kg)?",
         "Canned baby food or milk powder (number of cans)?",
         "Soup/noodles (number of cans)?", "Other goods?") %>%
  
  # Join with the table with coordinates for each unique village
  inner_join(UniqueVillages, by = "Village/town")

# Rename all TCI locations to "TCI"
VillageData$`Issuer Organization`[which(VillageData$`Issuer Organization` == "TCI - Chernivtsi")] = "TCI"
VillageData$`Issuer Organization`[which(VillageData$`Issuer Organization` == "TCI - Ivano-frankivsk")] = "TCI"
VillageData$`Issuer Organization`[which(VillageData$`Issuer Organization` == "TCI - Mykolajiv")] = "TCI"

# Rename all cities with no coordinates to "others" and give them the correct coordinates
VillageData = VillageData %>% 
  mutate(`Village/town` = if_else(`Issuer Organization` == "TCI" & is.na(lat), "Others, TCI", `Village/town`)) %>% 
  mutate(`Village/town` = if_else(`Issuer Organization` == "Pilgrim - Zaporizja" & is.na(lat), "Others, Pilgrim - Zaporizja", `Village/town`)) %>% 
  mutate(`Village/town` = if_else(`Issuer Organization` == "FriendsExist! - Dubno" & is.na(lat), "Others, FriendsExist! - Dubno", `Village/town`)) %>% 
  mutate(`Village/town` = if_else(`Issuer Organization` == "New Life Centre - Kyiv" & is.na(lat), "Others, New Life Centre - Kyiv", `Village/town`)) %>%
  mutate(lat = if_else(is.na(lat) & `Issuer Organization` == "TCI", 31.9692583, lat)) %>% 
  mutate(lng = if_else(is.na(lng) & `Issuer Organization` == "TCI", 46.9607107, lng)) %>% 
  mutate(lat = if_else(is.na(lat) & `Issuer Organization` == "Pilgrim - Zaporizja", 35.1806849, lat)) %>% 
  mutate(lng = if_else(is.na(lng) & `Issuer Organization` == "Pilgrim - Zaporizja", 47.8511949, lng)) %>% 
  mutate(lat = if_else(is.na(lat) & `Issuer Organization` == "FriendsExist! - Dubno", 25.7349241, lat)) %>% 
  mutate(lng = if_else(is.na(lng) & `Issuer Organization` == "FriendsExist! - Dubno", 50.4221361, lng)) %>% 
  mutate(lat = if_else(is.na(lat) & `Issuer Organization` == "New Life Centre - Kyiv", 30.5008256, lat)) %>% 
  mutate(lng = if_else(is.na(lng) & `Issuer Organization` == "New Life Centre - Kyiv", 50.4502522, lng))  

# Get sum of food delivered per city
TotalDelivered <- VillageData %>% 
  
  # Group by city
  group_by(`Village/town`) %>% 
  
  # Summarise totals per village
  summarise(`Issuer Organization` = first(`Issuer Organization`),
            `Total families` = n_distinct(`Family name`),
            `Total persons` = sum(`Number of people in household`),
            `Total children` = sum(`Of which children (< 18 years)`),
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
  inner_join(UniqueVillagesWithOthers, by = "Village/town", ) %>% 
  
  # Remove column country
  dplyr::select(-country)

# Rename all rows of TCI villages where only 1 family received aid to "Others, TCI"
TotalDelivered <- TotalDelivered %>% 
  mutate(`Village/town` = if_else((`Total families` == 1 | `Total families` == 2) & `Issuer Organization` == "TCI", "Others, TCI", `Village/town`)) %>% 
  mutate(lat = if_else(`Village/town` == "Others, TCI", 31.9692583, lat)) %>% 
  mutate(lng = if_else(`Village/town` == "Others, TCI", 46.9607107, lng))

# Group cities again
TotalDelivered <- TotalDelivered %>% 
  
  # Group by city
  group_by(`Village/town`) %>% 
  
  # Summarise totals per village
  summarise(`Issuer Organization` = first(`Issuer Organization`),
            `Total families` = sum(`Total families`),
            `Total persons` = sum(`Total persons`),
            `Total children` = sum(`Total children`),
            `Total potato (kg)` = sum(`Total potato (kg)`),
            `Vegetables (kg)` = sum(`Vegetables (kg)`),
            `Total potato seeds (kg)` = sum(`Total potato seeds (kg)`),
            `Total plant onions seeds (kg)` = sum(`Total plant onions seeds (kg)`),
            `Total vegetables seeds (n bags)` = sum(`Total vegetables seeds (n bags)`),
            `Total flower (kg)` = sum(`Total flower (kg)`),
            `Total canned fish/meat (n cans)` = sum(`Total canned fish/meat (n cans)`),
            `Total salt/sugar (kg)` = sum(`Total salt/sugar (kg)`),
            `Total pasta/rice (kg)` = sum(`Total pasta/rice (kg)`),
            `Total baby food/milk powder (n cans)` = sum(`Total baby food/milk powder (n cans)`),
            `Total soup/noodles (n cans)` = sum(`Total soup/noodles (n cans)`)) %>% 
  
  # Join with unique villages for coordinates
  inner_join(UniqueVillagesAdapted, by = "Village/town", )

# Make totals spatial
TotalPerCity <- st_as_sf(TotalDelivered, coords = c("lng", "lat"), crs = st_crs(4326))

# Remove cities with weird names or locations
TotalPerCity <- TotalPerCity[!(TotalPerCity$`Village/town`=="234" | TotalPerCity$`Village/town`=="3"
                               | TotalPerCity$`Village/town`=="4" | TotalPerCity$`Village/town`=="77"
                               | TotalPerCity$`Village/town`=="98" | TotalPerCity$`Village/town`== "Білокуракине"
                               | TotalPerCity$`Village/town`=="Ганусівка" | TotalPerCity$`Village/town`== "Луганськ"
                               | TotalPerCity$`Village/town`=="Павлівка" | TotalPerCity$`Village/town`=="Демьяновка"
                               | TotalPerCity$`Village/town`=="Матвеевка" | TotalPerCity$`Village/town`=="Новоалександровка"
                               | TotalPerCity$`Village/town`=="Н"),]

# Add lgn and lat column to TotalPerCity
TotalPerCity$x <- st_coordinates(TotalPerCity)[,1]
TotalPerCity$y <- st_coordinates(TotalPerCity)[,2]

# Add latin city name as column
TotalPerCityLatine <- TotalPerCity %>% 
  
  # Ukrainian cyrillic to latin
  mutate(VillageNameLatin = stri_trans_general(str = `Village/town`, id = "Ukrainian-Latin/BGN"))

# Pilgrim - Zaporizja has made some mistakes in the pasta/rice data. I change the 
# weirdest 17 ones to 0, to get more normal values
TotalPerCityCorrection <- TotalPerCityLatine %>% 
  dplyr::mutate(`Total pasta/rice (kg)` = if_else(`Total pasta/rice (kg)` > 10000, 0, `Total pasta/rice (kg)`)) %>% 
  dplyr::mutate(`Total pasta/rice (kg)` = if_else(`Village/town` == "Others, Pilgrim - Zaporizja", 1470, `Total pasta/rice (kg)`))

# Visualize
plot(st_coordinates(TotalPerCityCorrection))

# Get list of villages/towns on implausible locations
ImplausibleLocations <- TotalPerCity %>% 
  
  # Filter all rows with implausible lng and lat
  dplyr::filter(`Village/town` == "Великий курінь" |
                  `Village/town` == "Зарічне" |
                  `Village/town` == "Залішани"|
                  `Village/town` == "Кузьмівка"|
                  `Village/town` == "Славне"|
                  `Village/town` == "Мар'янівка"|
                  `Village/town` == "Соснівка"|
                  `Village/town` == "Соколивка"|
                  `Village/town` == "Круча"|
                  `Village/town` == "Городок"|
                  `Village/town` == "Болотна"|
                  `Village/town` == "Бортники"|
                  `Village/town` == "Великий"|
                  `Village/town` == "Максимовичі"|
                  `Village/town` == "Поляна"|
                  `Village/town` == "Мирча"|
                  `Village/town` == "Іванівці"|
                  `Village/town` == "Заріччя"|
                  `Village/town` == "Тарасівка"|
                  `Village/town` == "Плоска"|
                  `Village/town` == "Макарівка"|
                  `Village/town` == "Губин"|
                  `Village/town` == "Мостище"|
                  `Village/town` == "Н"|
                  `Village/town` == "Новоукраїнка"|
                  `Village/town` == "Доманівка"|
                  `Village/town` == "Привільне"|
                  `Village/town` == "Заводской район"|
                  `Village/town` == "Чернігів"|
                  `Village/town` == "Киселівка"|
                  `Village/town` == "Н-ОЛЕКСАНДРІВКА"|
                  `Village/town` == "Зоряне"|
                  `Village/town` == "Зелений гай"|
                  `Village/town` == "Суми"|
                  `Village/town` == "Новопетровка"|
                  `Village/town` == "Бахмут"|
                  `Village/town` == "Часів Яр"|
                  `Village/town` == "Торецьк"|
                  `Village/town` == "Авдіївка"|
                  `Village/town` == "Донецьк"|
                  `Village/town` == "Макіївка"|
                  `Village/town` == "Межове"|
                  `Village/town` == "Сніжне"|
                  `Village/town` == "Гірськє"|
                  `Village/town` == "Голубівка"|
                  `Village/town` == "Попасна"|
                  `Village/town` == "Калинове"|
                  `Village/town` == "Артемівськ"|
                  `Village/town` == "Перевальськ"|
                  `Village/town` == "Федорівка"|
                  `Village/town` == "Маріуполь"|
                  `Village/town` == "Бердянськ")

## Split implausible location city list per issuer organization

# TCI
TCIImplausibleLocations <- ImplausibleLocations %>% 
  
  # Select TCI
  filter(`Issuer Organization` == "TCI")

# FriendsExist!
FriendsExistImplausibleLocations <- ImplausibleLocations %>% 
  
  # Select FriendsExist
  filter(`Issuer Organization` == "FriendsExist! - Dubno")

# Pilgrim
PilgrimImplausibleLocations <- ImplausibleLocations %>% 
  
  # Select Pilgrim
  filter(`Issuer Organization` == "Pilgrim - Zaporizja")

# New Life Centre
NewLifeCentreImplausibleLocations <- ImplausibleLocations %>% 
  
  # Select New Life Centre
  filter(`Issuer Organization` == "New Life Centre - Kyiv")


## Get data per Issuer Organization

# Summarize all data per Issuer Organization
IssuerOrganizationSummary <- TotalPerCityCorrection %>% 
  
  # Group by city
  group_by(`Issuer Organization`) %>% 
  
  # Summarise totals per village
  summarise(`Total families` = sum(`Total families`),
            `Total persons` = sum(`Total persons`),
            `Total children` = sum(`Total children`),
            `Total potato (kg)` = sum(`Total potato (kg)`),
            `Vegetables (kg)` = sum(`Vegetables (kg)`),
            `Total potato seeds (kg)` = sum(`Total potato seeds (kg)`),
            `Total plant onions seeds (kg)` = sum(`Total plant onions seeds (kg)`),
            `Total vegetables seeds (n bags)` = sum(`Total vegetables seeds (n bags)`),
            `Total flower (kg)` = sum(`Total flower (kg)`),
            `Total canned fish/meat (n cans)` = sum(`Total canned fish/meat (n cans)`),
            `Total salt/sugar (kg)` = sum(`Total salt/sugar (kg)`),
            `Total pasta/rice (kg)` = sum(`Total pasta/rice (kg)`),
            `Total baby food/milk powder (n cans)` = sum(`Total baby food/milk powder (n cans)`),
            `Total soup/noodles (n cans)` = sum(`Total soup/noodles (n cans)`)) %>% 
  
  # Remove redundant geometry column
  tibble() %>% 
  dplyr::select(-geometry)

# Add lgn and lat column to TotalPerCity
IssuerOrganizationSummary$x <- c(25.73432, 30.523333, 35.19031, 32.0) 
IssuerOrganizationSummary$y <- c(50.41694, 50.450001, 47.82289, 46.9666628)


## Write pre-processed data to csv

# Create path
path <- "~/WorldPartnersHelp/Output/"

# Create directory
if(!dir.exists(path)){
  dir.create(path)
}

# Write important datafiles to csv
write_csv(TotalPerCityCorrection, file = paste0(path, "TotalsPerCity", ".csv"))
write_csv(NoCoordinateVillages, file = paste0(path, "VillagesWithoutCoordinates", ".csv"))
write_csv(IssuerOrganizationSummary, file = paste0(path, "IssuerOrganizationData", ".csv"))
write_csv(TCIImplausibleLocations, file = paste0(path, "TCIImplausibleLocations", ".csv"))
write_csv(FriendsExistImplausibleLocations, file = paste0(path, "FriendsExistImplausibleLocations", ".csv"))
write_csv(PilgrimImplausibleLocations, file = paste0(path, "PilgrimImplausibleLocations", ".csv"))
write_csv(NewLifeCentreImplausibleLocations, file = paste0(path, "NewLifeCentreImplausibleLocations", ".csv"))


