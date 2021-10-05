#'
#' @title Finds adjacent locations in a known locations table.
#' 
#' @description Calculates distances between all locations within a known
#' locations table and returns a tibble with the row indices and separation 
#' distances of those records separated by less than \code{distanceThreshold} meters.
#' 
#' It is useful when working with new metadata tables to identify ajacent
#' locations early on so that decisions can be made about the appropriateness
#' of the specified \code{distanceThreshold}.
#' 
#' @note The measure \code{"cheap"} may be used to speed things up depending on
#' the spatial scale being considered. Distances calculated with 
#' \code{measure = "cheap"} will vary by a few meters compared with those 
#' calculated using \code{measure = "geodesic"}.
#' 
#' @param locationTbl Tibble of known locations.
#' @param distanceThreshold Distance in meters.
#' @param measure One of "haversine" "vincenty", "geodesic", or "cheap" 
#' specifying desired method of geodesic distance calculation. See \code{?geodist::geodist}.
#' 
#' @return Tibble of known locations separated by less than 
#' \code{distanceThreshold} meters.
#' 
#' @examples 
#' library(MazamaLocationUtils)
#' 
#' meta <- wa_airfire_meta
#' 
#' # Any locations closer than 2 km?
#' meta %>%
#'   table_findAdjacentLocations(distanceThreshold = 2000) %>%
#'   dplyr::select(monitorID, siteName, timezone)
#' 
#' # How about 4 km?
#' meta %>%
#'   table_findAdjacentLocations(distanceThreshold = 4000) %>%
#'   dplyr::select(monitorID, siteName, timezone)
#' 
#' @rdname table_findAdjacentLocations
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_findAdjacentLocations <- function(
  locationTbl = NULL,
  distanceThreshold = NULL,
  measure = "geodesic"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  
  if ( !is.numeric(distanceThreshold) )
    stop("Parameter 'distanceThreshold' must be a numeric value.")
  
  # ----- Subset locationTbl ---------------------------------------------------
  
  adjacentDistanceTbl <- table_findAdjacentDistances(locationTbl, distanceThreshold, measure)
  indices <- c(dplyr::pull(adjacentDistanceTbl, 1), dplyr::pull(adjacentDistanceTbl, 2))
  adjacentLocationTbl <- locationTbl[indices,]
  
  # ----- Return ---------------------------------------------------------------
  
  return(adjacentLocationTbl)
  
}
