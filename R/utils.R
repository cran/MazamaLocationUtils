#' @title Validate proper setup of MazamaSpatialUtils
#' @description The \pkg{MazamaSpatialUtils} package mus be properly installed
#' and initialized before using functions from the \pkg{MazamaLocationUtils} 
#' package. Functions can test for this 
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateMazamaSpatialUtils
#' @export 
#' 
validateMazamaSpatialUtils <- function() {
  
  if ( !exists("EEZCountries") ||
       !exists("OSMTimezones") ||
       !exists("NaturalEarthAdm1") ||
       !exists("USCensusCounties") ) {
    
    stop(paste0(
      "\n\nYou must have the MazamaSpatialUtils package ",
      "as well as core datasets installed.\n\n",
      "Install core datasets with:\n\n",
      "  MazamaSpatialUtils::setSpatialDataDir(\"YOUR_DATA_DIR\")\n",
      "  MazamaSpatialUtils::installSpatialData()\n\n",
      "Once installed, initialize spatial data with:\n\n",
      "  MazamaSpatialUtils::setSpatialDataDir(\"YOUR_DATA_DIR\")\n",
      "  MazamaSpatialUtils::loadSpatialData(\"EEZCountries\")\n",
      "  MazamaSpatialUtils::loadSpatialData(\"OSMTimezones\")\n",
      "  MazamaSpatialUtils::loadSpatialData(\"NaturalEarthAdm1\")\n",
      "  MazamaSpatialUtils::loadSpatialData(\"USCensusCounties\")\n"
    ))
    
  }
  
  return(invisible(TRUE))
  
}


#' @title Validate longitude and latitude vectors
#' @description Longitude and latitude vectors validated to be parseable as numeric
#' and within the bounds -180:180 and -90:90. If validation fails, an error is
#' generated.
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param na.rm Logical specifying whether to remove \code{NA} values before
#' validation.
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateLonsLats
#' @export 
#' 
validateLonsLats <- function(
  longitude = NULL,
  latitude = NULL,
  na.rm = FALSE
) {
  
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  
  if ( length(longitude) != length(latitude) ) {
    stop(paste0(
      "longitude and latitude must have the same length"
    ))
  }
  
  # Remove locations with NAs
  if ( na.rm ) {
    good_mask <- !is.na(longitude) & !is.na(latitude)
    longitude <- longitude[good_mask]
    latitude <- latitude[good_mask]
  }
  
  longitude <- as.numeric(longitude)
  if ( anyNA(longitude) || any(longitude < -180) || any(longitude > 180 ))
    stop("all longitudes must be valid values between -180 and 180")
  
  latitude <- as.numeric(latitude)
  if ( anyNA(latitude) || any(latitude < -180) || any(latitude > 180) )
    stop("all latitudes must be a valid values between -180 and 180")
  
  return(invisible(TRUE))
  
}


#' @title Validate longitude and latitude values
#' @description Longitude and latitude are validated to be parseable as numeric
#' and within the bounds -180:180 and -90:90. If validation fails, an error is
#' generated.
#' @param longitude Single longitude in decimal degrees E.
#' @param latitude Single latitude in decimal degrees N.
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateLonLat
#' @export 
#' 
validateLonLat <- function(
  longitude = NULL,
  latitude = NULL
) {
  
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  
  if ( length(longitude) > 1 || length(latitude) > 1 ) {
    stop(paste0(
      "longitude and latitude must be single values"
    ))
  }
  
  longitude <- as.numeric(longitude)
  if ( is.na(longitude) || longitude < -180 || longitude > 180 )
    stop("longitude must be a valid value between -180 and 180")
  
  latitude <- as.numeric(latitude)
  if ( is.na(latitude) || latitude < -180 || latitude > 180 )
    stop("latitude must be a valid value between -180 and 180")
  
  return(invisible(TRUE))
  
}

#' @title Validate a location table
#' @description Ensures that the incoming table has numeric \code{longitude} and
#' \code{latitude} columns. 
#' \code{longitude} and latitude.
#' @param locationTbl Tibble of known locations.
#' @param locationOnly Logical specifying whether to check for all standard
#' columns.
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateLocationTbl
#' @export 
#' 
validateLocationTbl <- function(
  locationTbl = NULL,
  locationOnly = TRUE
) {
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  
  if ( !is.logical(locationOnly) ) locationOnly <- TRUE
  
  if ( !"data.frame" %in% class(locationTbl) )
    stop("Parameter 'locationTbl' is not of class \"data.frame\".")
  
  if ( !"longitude" %in% names(locationTbl) )
    stop("Parameter 'locationTbl' does not have a 'longitude' column.")
  
  if ( !"latitude" %in% names(locationTbl) )
    stop("Parameter 'locationTbl' does not have a 'latitude' column.")
  
  if ( !is.numeric(locationTbl$longitude) )
    stop("'locationTbl$longitude' is not numeric. Please ensure that decimal longitudes are used.")
  
  if ( !is.numeric(locationTbl$latitude) )
    stop("'locationTbl$latitude' is not numeric. Please ensure that decimal latitudes are used.")
  
  if ( !locationOnly ) {
    missingNames <- 
      setdiff(MazamaLocationUtils::coreMetadataNames, names(locationTbl))
    if ( length(missingNames) > 0 ) {
      missingNamesString <- paste0(missingNames, collapse = ", ")
      stop(sprintf("'locationTbl' is missing '%s'", missingNamesString))
    }
  }
  
  return(invisible(TRUE))
  
}

