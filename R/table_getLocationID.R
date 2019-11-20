
#' @title Return IDs of known locations
#' @description Returns a vector of \code{locationID}s for the known locations
#' that each incoming location will be assigned to within the given. If more
#' than one known location exists within the given radius, the closest will be
#' assigned.  \code{NA} will be returned for each incoming that cannot be 
#' assigend to a known location in \code{locationTbl}.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param longitude Vector of longitudes in decimal degrees E, Default: NULL
#' @param latitude Vector of latitudes in decimal degrees N, Default: NULL
#' @param radius Radius in meters, Default: NULL
#' @return Vector of known \code{locationID}s.
#' @examples
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' 
#' # Too small a radius will not find a match
#' table_getLocationID(locationTbl, lon, lat, radius = 50)
#' 
#' # Expanding the radius will find one
#' table_getLocationID(locationTbl, lon, lat, radius = 5000)
#' @rdname table_getLocationID
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom geodist geodist
#' @importFrom rlang .data
table_getLocationID <- function(
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
  
  # ----- Calculate distances --------------------------------------------------
  
  distance <-
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
      measure = "geodesic"
    )
  
  # NOTE:  distance matrix is nrow(locationTbl) X length(longitude)
  
  # ----- Find locationIDs -----------------------------------------------------
  
  locationID <- rep(as.character(NA), length(longitude))
  
  for ( index in seq_along(longitude) ) {
    
    if ( any(distance[,index] <= radius) ) {
      row <- which(distance[,index] == min(distance[,index]))
      locationID[index] <- locationTbl$locationID[row]
    }
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationID)
  
}
