options("pkgType" = "binary")

## Load packages ##
library(dplyr)
source("Map_plot.R")

## Variables ############
root <- ""
mapStyle <- "Country"
metric <- "Total_value"
mapDataLoc <- paste0(root,"countries.geo.json")
featureName <- "name"
analyticalDataLoc <- paste0(root,"cheese_sample_data")

# Load map data
mapData <- st_read(mapDataLoc)

# Load analytical data
analyticalData <- read.csv(analyticalDataLoc)

m <- Map_plot(mapData,
              featureName,
              analyticalData,
              mapStyle,
              metric,
              logScale = 10)
