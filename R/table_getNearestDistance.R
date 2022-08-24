#' @rdname table_getNearestDistance
#' @export
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom geodist geodist
#' @importFrom rlang .data
#' 
#' @title Return distances to nearest known locations
#' 
#' @param locationTbl Tibble of known locations.
#' @param longitude Vector of target longitudes in decimal degrees E.
#' @param latitude Vector of target latitudes in decimal degrees N.
#' @param distanceThreshold Distance in meters.
#' @param measure One of "haversine" "vincenty", "geodesic", or "cheap" 
#' specifying desired method of geodesic distance calculation. 
#' 
#' @description Returns distances between target locations and the closest
#' location found in \code{locationTbl} (if any). Target locations are specified 
#' with \code{longitude} and \code{latitude}.
#' 
#' For each target location, only a single distance to the closest known location 
#' is returned. If no known location is found within
#' \code{distanceThreshold}, the distance associated with that target location
#' will be \code{NA}. The length and order of resulting distances will match the
#' order of the incoming target locations.
#' 
#' @section Use Case:
#' You may have a set of locations of interest for which you want to assess whether
#' any monitoring locations are nearby. In this case, the locations of interest
#' will provide \code{longitude} and \code{latitude} while \code{locationTbl}
#' will be the known location table associated with the monitoring locations. 
#' 
#' The resulting vector of distances will tell you the distance, for each 
#' target location, to the nearst monitoring location.
#' 
#' @note The measure \code{"cheap"} may be used to speed things up depending on
#' the spatial scale being considered. Distances calculated with 
#' \code{measure = "cheap"} will vary by a few meters compared with those 
#' calculated using \code{measure = "geodesic"}.
#' 
#' See \code{geodist::\link[geodist:geodist]{geodist}} for details.
#' 
#' @return Vector of closest distances between target locations and known locations.
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
  # NOTE:  things up by only calculating distances for unique locations and then
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
