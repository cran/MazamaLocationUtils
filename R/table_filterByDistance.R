#' @rdname table_filterByDistance
#' @export
#' @importFrom MazamaCoreUtils stopIfNull
#' 
#' @title Return known locations near a target location
#' 
#' @param locationTbl Tibble of known locations.
#' @param longitude Target longitude in decimal degrees E.
#' @param latitude Target latitude in decimal degrees N.
#' @param distanceThreshold Distance in meters.
#' @param measure One of "haversine" "vincenty", "geodesic", or "cheap" 
#' specifying desired method of geodesic distance calculation. 
#' 
#' @return Tibble of known locations.
#' 
#' @description Returns a tibble of the known locations from \code{locationTbl} 
#' that are within \code{distanceThreshold} meters of the target location 
#' specified by \code{longitude} and \code{latitude}. 
#' 
#' @note Only a single target location is allowed.
#' 
#' @examples
#' library(MazamaLocationUtils)
#' 
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Too small a distanceThreshold will not find a match
#' locationTbl %>%
#'   table_filterByDistance(
#'     longitude = -117.3647, 
#'     latitude = 47.6725, 
#'     distanceThreshold = 10
#'   ) %>% 
#'   dplyr::glimpse()
#' 
#' # Expanding the distanceThreshold will find several
#' locationTbl %>%
#'   table_filterByDistance(
#'     longitude = -117.3647, 
#'     latitude = 47.6725, 
#'     distanceThreshold = 10000
#'   ) %>%
#'   dplyr::glimpse()

table_filterByDistance <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  distanceThreshold = NULL,
  measure = c("geodesic", "haversine", "vincenty", "cheap")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaLocationUtils::validateLonLat(longitude, latitude)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  measure <- match.arg(measure)
  
  distanceThreshold <- round(distanceThreshold)

  # ----- Calculate distances --------------------------------------------------
  
  distanceMatrix <-
    geodist::geodist(
      y = cbind(
        "x" = longitude,
        "y" = latitude
      ),
      x = cbind(
        "x" = locationTbl$longitude,
        "y" = locationTbl$latitude
      ),
      paired = FALSE,
      sequential = FALSE,
      pad = FALSE,
      measure = measure
    )
  
  # NOTE:  distanceMatrix is nrow(locationTbl) X 1
  
  distance <- distanceMatrix[,1]
  names(distance) <- locationTbl$locationID
  
  distance <- distance[distance <= distanceThreshold]
  
  if ( length(distance) == 0 ) {
    ids <- "DON'T FIND ME"
  } else {
    ids <-
      distance %>%
      sort() %>%
      names()
  }
  
  # ----- Subset ---------------------------------------------------------------

  subsetTbl <- 
    locationTbl %>%
    dplyr::filter(.data$locationID %in% ids)
  
  # ----- Return ---------------------------------------------------------------

  return(subsetTbl)

}
