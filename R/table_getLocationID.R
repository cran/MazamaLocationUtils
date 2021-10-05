
#' @title Return IDs of known locations
#' @description Returns a vector of \code{locationID}s for the known locations
#' that each incoming location will be assigned to within the given. If more
#' than one known location exists within the given \code{distanceThreshold}, the closest will be
#' assigned.  \code{NA} will be returned for each incoming that cannot be 
#' assigned to a known location in \code{locationTbl}.
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
#' specifying desired method of geodesic distance calculation. See \code{?geodist::geodist}.
#' @return Vector of known \code{locationID}s.
#' @examples
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' 
#' # Too small a distanceThreshold will not find a match
#' table_getLocationID(locationTbl, lon, lat, distanceThreshold = 50)
#' 
#' # Expanding the distanceThreshold will find one
#' table_getLocationID(locationTbl, lon, lat, distanceThreshold = 5000)
#' @rdname table_getLocationID
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom geodist geodist
#' @importFrom rlang .data
table_getLocationID <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  distanceThreshold = NULL,
  measure = "geodesic"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
  MazamaLocationUtils::validateLonsLats(longitude, latitude)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  
  if ( !is.numeric(distanceThreshold) )
    stop("Parameter 'distanceThreshold' must be a numeric value.")
  
  distanceThreshold <- round(distanceThreshold)
  
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
      measure = measure
    )

  # NOTE:  distance matrix is nrow(locationTbl) X length(longitude)

  # ----- Find locationIDs -----------------------------------------------------

  locationID <- rep(as.character(NA), length(longitude))

  for ( index in seq_along(longitude) ) {

    if ( any(distance[,index] <= distanceThreshold) ) {
      row <- which(distance[,index] == min(distance[,index]))
      locationID[index] <- locationTbl$locationID[row]
    }

  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationID)
  
}
