#' @rdname table_getDistanceFromTarget
#' @export
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom rlang .data
#' 
#' @title Return distances and directions from a target location to known locations
#' 
#' @param locationTbl Tibble of known locations.
#' @param longitude Target longitude in decimal degrees E.
#' @param latitude Target latitude in decimal degrees N.
#' @param measure One of "geodesic", "haversine", "vincenty" or "cheap" 
#' specifying desired method of geodesic distance calculation. 
#' 
#' @return Tibble of distances in meters and cardinal directions from a target location.
#' 
#' @description Returns a tibble with the same number of rows as \code{locationTbl} 
#' containing the distance and direction from the target location specified by 
#' \code{longitude} and \code{latitude} to each known location found in 
#' \code{locationTbl}. 
#' 
#' @note Only a single target location is allowed.
#' 
#' @examples
#' library(MazamaLocationUtils)
#' 
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' locationTbl %>%
#'   table_getDistanceFromTarget(
#'     longitude = -117.3647, 
#'     latitude = 47.6725
#'   ) %>% 
#'   dplyr::glimpse()
#' 


table_getDistanceFromTarget <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  measure = c("geodesic", "haversine", "vincenty", "cheap")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaLocationUtils::validateLonLat(longitude, latitude)
  measure <- match.arg(measure)
  
  if ( length(longitude) > 1 || length(latitude) > 1 ) 
    stop("Only a single target location is allowed.")
  
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
  
  returnTbl <- 
    locationTbl %>%
    dplyr::select(c("locationID")) %>%
    dplyr::mutate(
      distanceFromTarget = round(distanceMatrix[,1])
    )

  # ----- Add direction --------------------------------------------------------
  
  # https://stackoverflow.com/questions/30794729/find-angle-from-two-points-formula
  
  x1 <- longitude * pi/180
  x2 <- locationTbl$longitude * pi/180
  y1 <- latitude * pi/180
  y2 <- locationTbl$latitude * pi/180
  
  angle = atan2(y2 - y1, x2 - x1) * 180/pi
  
  # NOTE:  Mathematical angle is zero facing East and increases counter-clockwise
  # NOTE:  Cover everything between -180:180
  
  returnTbl$directionFromTarget <- 
    cut(
      angle,
      breaks = c(-202.5, -157.5, -112.5, -67.5, -22.5, 22.5, 67.5, 112.5, 157.5, 202.5),
      labels = c("W", "SW", "S", "SE", "E", "NE", "N", "NW", "W")
    ) %>%
    as.character()
  
  # ----- Return ---------------------------------------------------------------

  return(returnTbl)

}
