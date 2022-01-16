
#' @title Return distances to nearest known locations
#' @description Returns distances from known locations in \code{locationTbl},
#' one for each incoming location. If no known location is found within
#' \code{distanceThreshold} meters for a particular
#' incoming location, that distance in the vector will be \code{NA}.
#' 
#' @note The measure \code{"cheap"} may be used to speed things up depending on
#' the spatial scale being considered. Distances calculated with 
#' \code{measure = "cheap"} will vary by a few meters compared with those 
#' calculated using \code{measure = "geodesic"}.
#' 
#' @param locationTbl Tibble of known locations.
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param distanceThreshold Distance in meters.
#' @param measure One of "haversine" "vincenty", "geodesic", or "cheap" 
#' specifying desired method of geodesic distance calculation. 
#' 
#' See \code{geodist::\link[geodist:geodist]{geodist}} for details.
#' 
#' @return Vector of distances from known locations.
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
#' table_getNearestDistance(locationTbl, lon, lat, distanceThreshold = 50)
#' 
#' # Expanding the distanceThreshold will find one
#' table_getNearestDistance(locationTbl, lon, lat, distanceThreshold = 5000)
#' 
#' @rdname table_getNearestDistance
#' @export
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom geodist geodist
#' @importFrom rlang .data
table_getNearestDistance <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  distanceThreshold = NULL,
  measure = c("geodesic", "haversine", "vincenty", "cheap")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaLocationUtils::validateLonsLats(longitude, latitude)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  measure <- match.arg(measure)

  if ( !is.numeric(distanceThreshold) )
    stop("Parameter 'distanceThreshold' must be a numeric value.")
  
  distanceThreshold <- round(distanceThreshold)

  # ----- Find unique locations ------------------------------------------------
  
  # NOTE:  For the case where the incoming longitude and latitude have many 
  # NOTE:  duplicated locations (a "tidy" dataframe perhaps), we need to speed
  # NOTE:  things up by only calculating distance for unique locations and then
  # NOTE:  merging with all incoming locations.
  
  myTbl <- dplyr::tibble(longitude = longitude, latitude = latitude)
  
  myUniqueTbl <- myTbl %>% dplyr::distinct()
  
  # ----- Calculate distances --------------------------------------------------

  distance <-
    geodist::geodist(
      y = cbind(
        "x" = myUniqueTbl$longitude,
        "y" = myUniqueTbl$latitude
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

  # NOTE:  distance matrix is nrow(locationTbl) X nrow(myUniqueTbl)

  # ----- Find unique distances ------------------------------------------------
  
  myUniqueTbl$nearestDistance <- as.numeric(NA)
  
  for ( index in seq_len(nrow(myUniqueTbl)) ) {
    
    if ( any(distance[,index] <= distanceThreshold) ) {
      myUniqueTbl$nearestDistance[index] <-  min(distance[,index], na.rm = TRUE)
    }
    
  }
  
  # ----- Merge with all locations ---------------------------------------------
  
  nearestDistance <- 
    dplyr::full_join(
      myTbl,
      myUniqueTbl,
      by = c("longitude", "latitude")
    ) %>%
    dplyr::pull(.data$nearestDistance)
  
  # ----- Return ---------------------------------------------------------------

  return(nearestDistance)

}
