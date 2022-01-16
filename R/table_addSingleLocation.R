
#' @title Add a single new known location record to a table
#' 
#' @description Incoming \code{longitude} and \code{latitude} values are compared 
#' against the incoming \code{locationTbl} to see if they are already within
#' \code{distanceThreshold} meters of an existing entry.  A new record is created for
#' if the location is not already found in \code{locationTbl}.
#' 
#' @param locationTbl Tibble of known locations.
#' @param longitude Single longitude in decimal degrees E.
#' @param latitude Single latitude in decimal degrees N.
#' @param distanceThreshold Distance in meters.
#' @param stateDataset Name of spatial dataset to use for determining state
#' codes, Default: "NaturalEarthAdm1".
#' @param elevationService Name of the elevation service to use for determining
#' the elevation. Default: NULL. Accepted values: "usgs".
#' @param addressService Name of the address service to use for determining
#' the street address. Default: NULL. Accepted values: "photon".
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return Updated tibble of known locations.
#' 
#' @examples
#' \donttest{
#' library(MazamaLocationUtils)
#' 
#' # Fail gracefully if any resources are not available
#' try({
#' 
#'   # Set up standard directories and spatial data
#'   spatialDataDir <- tempdir() # typically "~/Data/Spatial"
#'   mazama_initialize(spatialDataDir)
#' 
#'   locationTbl <- get(data("wa_monitors_500"))
#' 
#'   # Coulee City, WA
#'   lon <- -119.290904
#'   lat <- 47.611942
#' 
#'   locationTbl <- 
#'     locationTbl %>%
#'     table_addSingleLocation(lon, lat, distanceThreshold = 500)
#'   
#' }, silent = FALSE)
#' }
#' 
#' @seealso \link{table_addLocation}
#' @seealso \link{table_removeRecord}
#' @seealso \link{table_updateSingleRecord}
#' @rdname table_addSingleLocation
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr bind_rows
#' 
table_addSingleLocation <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  distanceThreshold = NULL,
  stateDataset = "NaturalEarthAdm1",
  elevationService = NULL,
  addressService = NULL,
  verbose = TRUE
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  MazamaCoreUtils::stopIfNull(stateDataset)
  
  if ( length(longitude) > 1 || length(latitude) > 1 ) {
    stop(paste0(
      "Please use the plural version of the funcion for adding multiple locations.\n",
      "  table_addLocation(...)\n"
    ))
  }

  MazamaLocationUtils::validateLonLat(longitude, latitude)
  
  if ( !exists(stateDataset) ) {
    stop(paste0(
      "You must load \"stateDataset\" with: \n",
      "  loadSpatialData(\"", stateDataset, "\")\n"
    ))
  }
  
  # ----- Check for existing location ------------------------------------------
  
  locationID <- table_getLocationID(locationTbl, longitude, latitude, distanceThreshold)
  
  if ( !is.na(locationID) ) {
    stop(sprintf(
      "The known location %s already exists < %d meters from the requested location.",
      locationID, distanceThreshold
    ))
  }
  
  # ----- Add new record -------------------------------------------------------

  singleRecordTbl <- location_initialize(
    longitude = longitude,
    latitude = latitude,
    stateDataset = stateDataset,
    elevationService = elevationService,
    addressService = addressService,
    verbose = verbose
  )
    
  additionalNames <- setdiff( names(locationTbl), names(singleRecordTbl))
  
  for ( name in additionalNames ) {
    
    if ( verbose ) 
      message(sprintf("Using NA in place of actual metadata for %s", name))
    
    singleRecordTbl[[name]] <- NA
    
  }
  
  locationTbl <- dplyr::bind_rows(locationTbl, singleRecordTbl)
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
