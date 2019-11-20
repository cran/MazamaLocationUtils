
#' @title  Create known location record with core metadata
#' @description Creates a known location record with the following columns
#' of core metadata:
#' \itemize{
#' \item{locationID}
#' \item{locationName}
#' \item{longitude}
#' \item{latitude}
#' \item{elevation}
#' \item{countryCode}
#' \item{stateCode}
#' \item{county}
#' \item{timezone}
#' \item{houseNumber}
#' \item{street}
#' \item{city}
#' \item{zip}
#' }
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @param stateDataset Name of spatial dataset to use for determining state
#' @param verbose Logical controlling the generation of progress messages.
#' @return Tibble with a single new known location.
#' @rdname location_initialize
#' @examples
#' \donttest{
#' # Set up standard directories and spatial data
#' spatialDataDir <- tempdir() # typically "~/Data/Spatial"
#' mazama_initialize(spatialDataDir)
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' locationRecord <- location_initialize(lon, lat)
#' }
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom MazamaSpatialUtils getCountryCode getStateCode getTimezone
#' @importFrom dplyr tibble
#' @importFrom stringr str_sub
location_initialize <- function(
  longitude = NULL,
  latitude = NULL,
  stateDataset = "NaturalEarthAdm1",
  verbose = TRUE
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)
  
  MazamaCoreUtils::stopIfNull(stateDataset)

  if ( !exists(stateDataset) ) {
    stop(paste0(
      "You must load \"stateDataset\" with: \n",
      "  loadSpatialData(\"", stateDataset, "\")\n"
    ))
  }
  
  # ----- locationID, Elevation ------------------------------------------------

  locationID <- location_createID(
    longitude = longitude,
    latitude = latitude
  )
  
  elevation <- location_getSingleElevation_USGS(
    longitude = longitude,
    latitude = latitude,
    verbose = verbose
  )  

  # ----- Country, State, County, Timezone -------------------------------------
  
  countryCode <- MazamaSpatialUtils::getCountryCode(
    lon = longitude,
    lat = latitude,
    dataset = "EEZCountries",
    useBuffering = FALSE
  )
  
  stateCode <- MazamaSpatialUtils::getStateCode(
    lon = longitude,
    lat = latitude,
    dataset = stateDataset,
    useBuffering = TRUE
  )
  
  county <- MazamaSpatialUtils::getUSCounty(
    lon = longitude,
    lat = latitude,
    dataset = "USCensusCounties",
    stateCodes = stateCode,
    useBuffering = TRUE
  )
  
  timezone <- MazamaSpatialUtils::getTimezone(
    lon = longitude,
    lat = latitude,
    dataset = "OSMTimezones",
    useBuffering = TRUE
  )
  
  # ----- LocationName ---------------------------------------------------------
  
  # NOTE:  The default locationName is intended to give folks a more memorable
  # NOTE:  handel than the locationID but is not guaranteed to be unique. It is 
  # NOTE:  expected that users will add their own, more relevant names 
  # NOTE:  appropriate for the community of practice using a particular
  # NOTE:  collectionName of known locations.
  
  locationName <- paste0(
    tolower(countryCode), ".",
    tolower(stateCode), "_",
    stringr::str_sub(locationID, 1, 6)
  )
  
  # ----- Address --------------------------------------------------------------
  
  addressList <- location_getSingleAddress_Photon(
    longitude = longitude,
    latitude = latitude,
    verbose = verbose
  )
  
  # NOTE:  The Photon reverse geocoding service returns NA for countryCode
  # NOTE:  and stateCode when no specific address can be found. In these 
  # NOTE:  cases (remote areas) we want to use the MazamaSpatialUtils results.
  # NOTE:  Otherwise, we trust that Photon reverse geocoding is more accurate
  # NOTE:  than the point-in-polygon seraches done by MazamaSpatialUtils.
  
  if ( !is.na(addressList$countryCode) &&
       addressList$countryCode != countryCode ) {
    # Trust address data over MazamaSpatialUtils result
    if ( verbose ) {
      warning(sprintf(
        "Using address countryCode \"%s\" over MazamaSpatialUtils \"%s\"",
        addressList$countryCode, countryCode
      ))
    }
    countryCode <- addressList$countryCode
  }
  
  if ( !is.na(addressList$stateCode) &&
       addressList$stateCode != stateCode ) {
    
    # Trust address data over MazamaSpatialUtils result
    MazamaStateCode <- stateCode
    
    # Change stateCode and reset possibly incorrect county
    stateCode <- addressList$stateCode
    county <- as.character(NA)
    
    # Update locationName
    locationName <- paste0(
      tolower(countryCode), ".",
      tolower(stateCode), "_",
      stringr::str_sub(locationID, 1, 6)
    )
    
    if ( verbose ) {
      warning(sprintf(
        "For %s, using address stateCode \"%s\" over MazamaSpatialUtils \"%s\"",
        locationName, addressList$stateCode, MazamaStateCode
      ))
    }
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  locationTbl <- dplyr::tibble(
    "locationID" = locationID,
    "locationName" = locationName,
    "longitude" = longitude,
    "latitude" = latitude,
    "elevation" = elevation,
    "countryCode" = countryCode,
    "stateCode" = stateCode,
    "county" = county,
    "timezone" = timezone,
    "houseNumber" = addressList$houseNumber,
    "street" = addressList$street,
    "city" = addressList$city,
    "zip" = addressList$zip
  )
  
  return(locationTbl)
  
}