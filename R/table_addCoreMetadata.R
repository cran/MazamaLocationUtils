
#' @title Addes missing metadata columns a known location table
#' 
#' @description An existing table will be amended to guarantee that it includes
#' the following core metadata columns.
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
#' The \code{longitude} and \code{latitude} columns are required to exist in the
#' incoming tibble but all others are optional.
#' 
#' If any of these core metadata columns are found, they will be retained.
#' 
#' The \code{locationID} will be generated (anew if already found) from existing
#' longitude and latitude data.
#' 
#' Other core metadata columns will be filled with \code{NA} values of the 
#' proper type.
#' 
#' The result is a tibble with all of the core metadata columns. Theses columns
#' must then be filled in to create a usable "known locations" table.
#' 
#' @note No check is performed for overlapping locations. The returned tibble
#' has the structure of a "known locations" table and is a good starting place
#' investigation. But further work is required to produce a valid table of
#' "known locations" associated with a specific spatial scale.
#' 
#' @param locationTbl Tibble of known locations. This input tibble need not be a 
#' standardized "known location" with all required columns. They will be added.
#' 
#' @return Tibble with the metadata columns required in a "known locations" table.
#' 
#' @rdname table_addCoreMetadata
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_addCoreMetadata <- function(
  locationTbl = NULL
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)

  # ----- Create locationTbl ---------------------------------------------------
  
  incomingTblColumns <- names(locationTbl)
  
  # * locationID -----
  
  locationTbl$locationID <- location_createID(
    longitude = locationTbl$longitude,
    latitude = locationTbl$latitude
  )
  
  # * elevation -----
  
  if ( !"elevation" %in% incomingTblColumns ) {
    locationTbl$elevation <- as.numeric(NA)
  }
  
  # * countryCode -----
  
  if ( !"countryCode" %in% incomingTblColumns ) {
    locationTbl$countryCode <- as.character(NA)
  }
  
  # * stateCode -----
  
  if ( !"stateCode" %in% incomingTblColumns ) {
    locationTbl$stateCode <- as.character(NA)
  }
  
  # * locationName -----
  
  if ( !"locationName" %in% incomingTblColumns ) {
    locationTbl$locationName <- as.character(NA)
  }
  
  # * county -----
  
  if ( !"county" %in% incomingTblColumns ) {
    locationTbl$county <- as.character(NA)
  }
  
  # * timezone -----
  
  if ( !"timezone" %in% incomingTblColumns ) {
    locationTbl$timezone <- as.character(NA)
  }
  
  # * houseNumber -----
  
  if ( !"houseNumber" %in% incomingTblColumns ) {
    locationTbl$houseNumber <- as.character(NA)
  }
  
  # * street -----
  
  if ( !"street" %in% incomingTblColumns ) {
    locationTbl$street <- as.character(NA)
  }
  
  # * city -----
  
  if ( !"city" %in% incomingTblColumns ) {
    locationTbl$city <- as.character(NA)
  }
  
  # * zip -----
  
  if ( !"zip" %in% incomingTblColumns ) {
    locationTbl$zip <- as.character(NA)
  }
  
  # ----- Reorganize locationTbl -----------------------------------------------
  
  requiredColumns <- c(
    "locationID", "locationName", 
    "longitude", "latitude", "elevation", 
    "countryCode", "stateCode", "county", "timezone", 
    "houseNumber", "street", "city", "zip"
  )
  
  extraColumns <- setdiff(incomingTblColumns, requiredColumns)
  
  # This is the preferred order
  allColumns <- c(requiredColumns, extraColumns)
  
  # Reorder and convert to tibble
  locationTbl <- 
    dplyr::select(locationTbl, dplyr::all_of(allColumns)) %>%
    dplyr::as_tibble()
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
