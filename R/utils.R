#' @title Validate proper setup of MazamaSpatialUtils
#' @description The \pkg{MazamaSpatialUtils} package mus be properly installed
#' and initialized before using functions from the \pkg{MazamaLocationUtils} 
#' package. This function tests for this.
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
      "  MazamaSpatialUtils::loadSpatialData(\"EEZCountries.rda\")\n",
      "  MazamaSpatialUtils::loadSpatialData(\"OSMTimezones.rda\")\n",
      "  MazamaSpatialUtils::loadSpatialData(\"NaturalEarthAdm1.rda\")\n",
      "  MazamaSpatialUtils::loadSpatialData(\"USCensusCounties.rda\")\n"
    ))
    
  }
  
  return(invisible(TRUE))
  
}


#' @title Validate longitude and latitude vectors
#' @description
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:validateLonsLats]{validateLonsLats}} for details.
#'
#' @name validateLonsLats
#' @rdname validateLonsLats
#' @keywords internal
#' @export
#' @importFrom MazamaCoreUtils validateLonsLats
#' @usage validateLonsLats(longitude = NULL, latitude = NULL, na.rm = FALSE)
NULL


#' @title Validate longitude and latitude values
#' @description
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:validateLonLat]{validateLonLat}} for details.
#'
#' @name validateLonLat
#' @rdname validateLonLat
#' @keywords internal
#' @export
#' @importFrom MazamaCoreUtils validateLonLat
#' @usage validateLonLat(longitude = NULL, latitude = NULL)
NULL


#' @title Validate a location table
#' @description Ensures that the incoming table has numeric \code{longitude} and
#' \code{latitude} columns.
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

