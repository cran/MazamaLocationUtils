
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
#' @param measure One of "geodesic", "haversine", "vincenty" or "cheap" 
#' specifying desired method of geodesic distance calculation. 
#' See \code{\link[geodist]{geodist}}.
#' 
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
#' 
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
  measure = c("geodesic", "haversine", "vincenty", "cheap")
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
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

  # ----- Find unique locationIDs ----------------------------------------------

  myUniqueTbl$locationID <- as.character(NA)
  
  for ( index in seq_len(nrow(myUniqueTbl)) ) {

    if ( any(distance[,index] <= distanceThreshold, na.rm = TRUE) ) {
      row <- which(distance[,index] == min(distance[,index], na.rm = TRUE))
      myUniqueTbl$locationID[index] <- locationTbl$locationID[row]
    }

  }
  
  # ----- Merge with all locations ---------------------------------------------
  
  locationID <- 
    dplyr::full_join(
      myTbl,
      myUniqueTbl,
      by = c("longitude", "latitude")
    ) %>%
    dplyr::pull(.data$locationID)
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationID)
  
}

# ===== DEBUG ==================================================================

if ( FALSE ) {
  
  library(MazamaLocationUtils)
  
  locationTbl <- MazamaCoreUtils::loadDataFile(
    filename = "airnow_PM2.5_sites.rda",
    dataUrl = NULL,
    dataDir = "~/Data/known_locations"
  )
  
  airnow_data <- MazamaCoreUtils::loadDataFile(
    filename = "airnow_PM2.5_202111.rda",
    dataUrl = "https://data-monitoring1.airfire.org/monitoring-v2/airnow-latency/2021",
    dataDir = NULL
  )
  
  longitude <- airnow_data$longitude
  latitude <- airnow_data$latitude
  
  distanceThreshold <- 100
  measure <- "geodesic"
  
  
  locationID <- 
    table_getLocationID(
      locationTbl = locationTbl,
      longitude = longitude,
      latitude = latitude,
      distanceThreshold = distanceThreshold,
      measure = measure
    )
  
  distance <- 
    table_getNearestDistance(
      locationTbl = locationTbl,
      longitude = longitude,
      latitude = latitude,
      distanceThreshold = distanceThreshold,
      measure = measure
    )

  
}


