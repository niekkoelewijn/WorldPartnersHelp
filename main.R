# Food aid distributed to Ukrainian towns and villages 
# Commossioned by Worldparners
# Excecuded by Niek Koelewijn
# September 2022

## This project aims to map the towns and villages in Ukraine that are received
## food aid from the NGO Worldpartners. The maps should have the following 
## requirements: (1) visualize which towns and villages are aided by which
## distributions centers, (2) the number of aided families (probably per 
## village), and (3) the quantity and type of food delivered to the villages.

## The project will be carried out in three steps: (1) preprocessing of the
## input data from Worldpartners, (2)  preprocessing of other needed geo-data,
## and (3) creating the visualizations in leaflet

# Step 1: Preprocessing the input data from Worldpartners
source("~/WorldPartnersHelp/R/PreprocessWorldpartnersData.R")

# Step 2: Preprocessing other relevant geo-data
source("~/WorldPartnersHelp//R/ProprocessGeodata.R")

# Step 3: Creating the visualization
source("~/WorldPartnersHelp//R/Visualization.R")
