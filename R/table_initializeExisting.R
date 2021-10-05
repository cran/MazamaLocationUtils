
#' @title Converts an existing table into a known location table
#' 
#' @description An existing table may have much of the data that is needed
#' for a known location table. This function accepts an incoming table and
#' searches for required columns:
#' 
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
#' 
#' The \code{longitude} and \code{latitude} columns are required but all others
#' are optional.
#' 
#' If any of these optional columns are found, they will be used and the often 
#' slow and sometimes slightly inaccurate steps to generate that information
#' will be skipped for locations with that data. Any additional columns of 
#' information not part of the required core metadata will be retained.
#' 
#' This method skips the assignment of columns like \code{elevation} and all
#' address related fields that require web service requests.
#' 
#' Compared to initializing a brand new table and populating one record at a
#' time, this is a much faster way of creating a known location table from a
#' pre-existing table of metadata.
#' 
#' @note The measure \code{"cheap"} may be used to speed things up depending on
#' the spatial scale being considered. Distances calculated with 
#' \code{measure = "cheap"} will vary by a few meters compared with those 
#' calculated using \code{measure = "geodesic"}.
#' 
#' @param locationTbl Tibble of known locations. This input tibble need not be a 
#' standardized "known location" with all required columns. They will be added.
#' @param stateDataset Name of spatial dataset to use for determining state
#' codes, Default: 'NaturalEarthAdm1'
#' @param countryCodes Vector of country codes used to optimize spatial
#' searching. (See ?MazamaSpatialUtils::getStateCode())
#' @param distanceThreshold Distance in meters. 
#' @param measure One of "haversine" "vincenty", "geodesic", or "cheap" 
#' specifying desired method of geodesic distance calculation. See \code{?geodist::geodist}.
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return Known location tibble with the specified metadata columns. Any 
#' locations whose circles (as defined by \code{distanceThreshold}) overlap will generate
#' warning messages. 
#' 
#' It is incumbent upon the user to address these issue by one of:
#' 
#' \enumerate{
#' \item{reduce the distanceThreshold until no overlaps occur}
#' \item{assign one of the overlapping locations to the other location}
#' }
#' 
#' @rdname table_initializeExisting
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_initializeExisting <- function(
  locationTbl = NULL,
  stateDataset = "NaturalEarthAdm1",
  countryCodes = NULL,
  distanceThreshold = NULL,
  measure = "geodesic",
  verbose = TRUE
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  
  if ( !exists(stateDataset) ) {
    stop(paste0(
      "You must load \"stateDataset\" with: \n",
      "  loadSpatialData(\"", stateDataset, "\")\n"
    ))
  }
  
  if ( "locationID" %in% names(locationTbl) )
    stop("Parameter 'locationTbl' already has a column named \"locationID\"")
  
  if ( !is.numeric(distanceThreshold) )
    stop("Parameter 'distanceThreshold' must be a numeric value.")
  
  # ----- Create locationTbl ---------------------------------------------------
  
  locationTbl <- table_addCoreMetadata(locationTbl)
  
  # * locationID -----
  
  # locationID should have been added by table_add
  if (anyNA(locationTbl$locationID)) {
    locationTbl$locationID <- location_createID(
      longitude = locationTbl$longitude,
      latitude = locationTbl$latitude
    )
  }
  
  # * elevation -----
  
  # Slow web service so skip for now
  
  # * countryCode -----
  
  tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$countryCode))
  tbl_2 <- dplyr::filter(locationTbl, is.na(.data$countryCode))
  
  if ( nrow(tbl_2) > 0 ) {
    
    if ( verbose ) 
      message(sprintf("Creating countryCodes for %d locations ...", nrow(tbl_2)))
    
    tbl_2$countryCode <- MazamaSpatialUtils::getCountryCode(
      lon = tbl_2$longitude,
      lat = tbl_2$latitude,
      dataset = "EEZCountries",
      countryCodes = countryCodes,
      useBuffering = FALSE
    )
    
    locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
    
  }
  
  # * stateCode -----
  
  tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$stateCode))
  tbl_2 <- dplyr::filter(locationTbl, is.na(.data$stateCode))
  
  if ( nrow(tbl_2) > 0 ) {
    
    if ( verbose ) 
      message(sprintf("Creating stateCodes for %d locations ...", nrow(tbl_2)))
    
    tbl_2$stateCode <- MazamaSpatialUtils::getStateCode(
      lon = tbl_2$longitude,
      lat = tbl_2$latitude,
      dataset = stateDataset,
      countryCodes = countryCodes,
      useBuffering = TRUE
    )
    
    locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
  
  }
  
  # * locationName -----
  
  # NOTE:  The default locationName is intended to give folks a more memorable
  # NOTE:  handel than the locationID but is not guaranteed to be unique. It is 
  # NOTE:  expected that users will add their own, more relevant names 
  # NOTE:  appropriate for the community of practice using a particular
  # NOTE:  collectionName of known locations.
  
  tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$locationName))
  tbl_2 <- dplyr::filter(locationTbl, is.na(.data$locationName))
  
  if ( nrow(tbl_2) > 0 ) {
    
    if ( verbose ) 
      message(sprintf("Creating locationNames for %d locations ...", nrow(tbl_2)))
    
    tbl_2$locationName <- paste0(
      tolower(tbl_2$countryCode), ".",
      tolower(tbl_2$stateCode), "_",
      stringr::str_sub(tbl_2$locationID, 1, 6)
    )
    
    locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
    
  }
  
  # * county -----
  
  # Slow so skip for now
  
  # tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$county))
  # tbl_2 <- dplyr::filter(locationTbl, is.na(.data$county))
  # 
  # if ( nrow(tbl_2) > 0 ) {
  #   
  #   if ( verbose ) 
  #     message(sprintf("Creating counties for %d locations ...", nrow(tbl_2)))
  #   
  #   tbl_2$county <- MazamaSpatialUtils::getUSCounty(
  #     lon = tbl_2$longitude,
  #     lat = tbl_2$latitude,
  #     dataset = "USCensusCounties",
  #     useBuffering = TRUE
  #   )
  #   
  #   locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
  #   
  # }
  
  # * timezone -----

  tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$timezone))
  tbl_2 <- dplyr::filter(locationTbl, is.na(.data$timezone))
  
  if ( nrow(tbl_2) > 0 ) {
    
    if ( verbose ) 
      message(sprintf("Creating timezones for %d locations ...", nrow(tbl_2)))
    
    tbl_2$county <- MazamaSpatialUtils::getTimezone(
      lon = tbl_2$longitude,
      lat = tbl_2$latitude,
      dataset = "OSMTimezones",
      useBuffering = TRUE
    )
    
    locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
    
  }
  
  # * houseNumber -----
  
  # Slow web service so skip for now
  
  # * street -----
  
  # Slow web service so skip for now
  
  # * city -----
  
  # Slow web service so skip for now

  # * zip -----
  
  # Slow web service so skip for now

  # ----- Check for adjaceent locations ----------------------------------------
  
  # # Calculate distances between each location
  # distances <- geodist::geodist(locationTbl, measure = "geodesic")
  # 
  # # Get distances that are less than the given distanceThreshold
  # # NOTE: the distance between a location and itself is always zero
  # distancesLessThanR <- (distances != 0) & (distances < distanceThreshold )
  # 
  # # Select the locations that are "adjacent".
  # overlappingTbl <- which(distancesLessThanR > 0, arr.ind = TRUE)
  
  overlappingTbl <- table_findAdjacentDistances(locationTbl, distanceThreshold, measure)

  if ( nrow(overlappingTbl) > 0 ) {
    
    overlappingCount <- nrow(overlappingTbl)
    
    # Format the first line of the warning message
    firstLine <- sprintf(
      "%d locations have neighbors that are < %d m apart\n",
      round(overlappingCount),
      distanceThreshold
    )
    
    # Create a vector of lines, on for each overlappingTbl location pair
    overlappingLines <- vector("character", length = overlappingCount)
    for ( i in seq_len(nrow(overlappingTbl)) ) {
      
      overlappingLines[i] <- sprintf(
        "Distance: %6.1f -- rows %s %s",
        round(overlappingTbl[i, 3], 1),
        overlappingTbl[i, 1],
        overlappingTbl[i, 2]
      )
      
    }
    
    instructions <- sprintf("
The presence of locations closer than twice the specified distanceThreshold invalidate the 
uniqueness of a 'known locations' table and should be rectified. There are several 
basic options:

  1) Reduce the distanceThreshold to less than the half the minimum distance.
  2) Manually remove one location from each pair.
  3) Manually merge nearby locations to share the same longitude, latitude and
     locationID
     
Please review the returned locationTbl for the identified rows with:

locationTbl %%>%%
  table_findAdjacentLocations(distanceThreshold = %d, measure = \"%s\") %%>%%
  table_leaflet()

  ", round(distanceThreshold), measure)
    
    lines <- c(firstLine, overlappingLines, instructions)
    
    # Paste the lines together
    warning(paste(lines, collapse = "\n"))
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
