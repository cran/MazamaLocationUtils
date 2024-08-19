## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 5)

## ----load_data----------------------------------------------------------------
wa <- get(data("wa_airfire_meta", package = "MazamaLocationUtils"))
names(wa)

## ----load_data_hidden, eval = TRUE, echo = FALSE------------------------------
library(MazamaLocationUtils)
wa_monitors_500 <- 
  get(data("wa_monitors_500", package = "MazamaLocationUtils")) %>%
  dplyr::mutate(elevation = as.numeric(NA))

## ----create_table, eval = FALSE, echo = TRUE----------------------------------
#  library(MazamaLocationUtils)
#  
#  # Initialize with standard directories
#  initializeMazamaSpatialUtils()
#  setLocationDataDir("./data")
#  
#  wa_monitors_500 <-
#    table_initialize() %>%
#    table_addLocation(wa$longitude, wa$latitude, distanceThreshold = 500)

## ----basic_columns------------------------------------------------------------
dplyr::glimpse(wa_monitors_500, width = 75)

## ----import_colmns------------------------------------------------------------
# Use a subset of the wa metadata
wa_indices <- seq(5,65,5)
wa_sub <- wa[wa_indices,]

# Use a generic name for the location table
locationTbl <- wa_monitors_500

# Find the location IDs associated with our subset
locationID <- table_getLocationID(
  locationTbl, 
  longitude = wa_sub$longitude, 
  latitude = wa_sub$latitude, 
  distanceThreshold = 500
)

# Now add the "AQSID" column for our subset of locations
locationData <- wa_sub$AQSID
locationTbl <- table_updateColumn(
  locationTbl, 
  columnName = "AQSID", 
  locationID = locationID, 
  locationData = locationData
)

# Lets see how we did
locationTbl_indices <- table_getRecordIndex(locationTbl, locationID)
locationTbl[locationTbl_indices, c("city", "AQSID")]

## ----new_locations------------------------------------------------------------
# Create new locations near our known locations
lons <- jitter(wa_sub$longitude) 
lats <- jitter(wa_sub$latitude)

# Any known locations within 50 meters?
table_getNearestLocation(
  wa_monitors_500,
  longitude = lons,
  latitude = lats,
  distanceThreshold = 50
) %>% dplyr::pull(city)

# Any known locations within 250 meters
table_getNearestLocation(
  wa_monitors_500,
  longitude = lons,
  latitude = lats,
  distanceThreshold = 250
) %>% dplyr::pull(city)

# How about 5000 meters?
table_getNearestLocation(
  wa_monitors_500,
  longitude = lons,
  latitude = lats,
  distanceThreshold = 5000
) %>% dplyr::pull(city)


## ----MSU_setup, echo = TRUE, eval = FALSE-------------------------------------
#    library(MazamaSpatialUtils)
#    setSpatialDataDir("~/Data/Spatial")
#  
#    installSpatialData("EEZCountries")
#    installSpatialData("OSMTimezones")
#    installSpatialData("NaturalEarthAdm1")
#    installSpatialData("USCensusCounties")

## ----standard_setup, echo = TRUE, eval = FALSE--------------------------------
#    MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
#  
#    MazamaSpatialUtils::loadSpatialData("EEZCountries.rda")
#    MazamaSpatialUtils::loadSpatialData("OSMTimezones.rda")
#    MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1.rda")
#    MazamaSpatialUtils::loadSpatialData("USCensusCounties.rda")

## ----easy_setup, echo = TRUE, eval = FALSE------------------------------------
#    library(MazamaLocationUtils)
#    initializeMazamaSpatialData()
#    setLocationDataDir("~/Data/KnownLocations")

