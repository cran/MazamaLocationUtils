
#' @title Return known locations
#' @description Returns a tibble of known locations from \code{locationTbl}, one 
#' for each incoming location. If no known location is found for a particular
#' incoming location, that record in the tibble will contain all \code{NA}.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param longitude Vector of longitudes in decimal degrees E, Default: NULL
#' @param latitude Vector of latitudes in decimal degrees N, Default: NULL
#' @param radius Radius in meters, Default: NULL
#' @return Tibble of known locations.
#' @examples
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' 
#' # Too small a radius will not find a match
#' table_getNearestLocation(locationTbl, lon, lat, radius = 50) %>% str()
#' 
#' # Expanding the radius will find one
#' table_getNearestLocation(locationTbl, lon, lat, radius = 5000) %>% str()
#' @rdname table_getNearestLocation
#' @export
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom geodist geodist
#' @importFrom rlang .data
table_getNearestLocation <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)

  radius <- round(radius)

  # ----- Subset ---------------------------------------------------------------

  incomingIDTbl <- dplyr::tibble(
    locationID = table_getLocationID(locationTbl, longitude, latitude, radius)
  )
  
  subsetTbl <- dplyr::left_join(incomingIDTbl, locationTbl, by = "locationID")

  # ----- Return ---------------------------------------------------------------

  return(subsetTbl)

}
