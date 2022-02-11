#' @rdname table_getNearestLocation
#' @export
#' @importFrom MazamaCoreUtils stopIfNull
#' 
#' @title Return known locations
#' 
#' @param locationTbl Tibble of known locations.
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param distanceThreshold Distance in meters.
#' 
#' @return Tibble of known locations.
#' 
#' @description Returns a tibble of the known locations from \code{locationTbl} 
#' that are closest to the vector of target locations specified by \code{longitude}
#' and \code{latitude}. Only a single known location is returned for each
#' incoming target location. If no known location is found for a particular
#' incoming location, that record in the tibble will contain all \code{NA}.
#' 
#' @examples
#' library(MazamaLocationUtils)
#' 
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' 
#' # Too small a distanceThreshold will not find a match
#' table_getNearestLocation(locationTbl, lon, lat, distanceThreshold = 50) %>% str()
#' 
#' # Expanding the distanceThreshold will find one
#' table_getNearestLocation(locationTbl, lon, lat, distanceThreshold = 5000) %>% str()

table_getNearestLocation <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  distanceThreshold = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaLocationUtils::validateLonsLats(longitude, latitude)
  MazamaCoreUtils::stopIfNull(distanceThreshold)

  distanceThreshold <- round(distanceThreshold)

  # ----- Subset ---------------------------------------------------------------

  incomingIDTbl <- dplyr::tibble(
    locationID = table_getLocationID(locationTbl, longitude, latitude, distanceThreshold)
  )
  
  subsetTbl <- dplyr::left_join(incomingIDTbl, locationTbl, by = "locationID")

  # ----- Return ---------------------------------------------------------------

  return(subsetTbl)

}
