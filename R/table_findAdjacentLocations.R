#'
#' @title Finds adjacent locations in a known locations table.
#' 
#' @description Calculate distances between all locations within a known
#' locations table and return a tibble containing all records that have an 
#' adjacent location separated by less than \code{distanceThreshold} meters.
#' The return tibble is ordered by separation distance.
#' 
#' It is useful when working with new metadata tables to identify adjacent
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
#' specifying desired method of geodesic distance calculation.
#' 
#' See \code{geodist::\link[geodist:geodist]{geodist}} for details.
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
#'   dplyr::select(siteName, timezone)
#' 
#' # How about 4 km?
#' meta %>%
#'   table_findAdjacentLocations(distanceThreshold = 4000) %>%
#'   dplyr::select(siteName, timezone)
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
  measure = c("geodesic", "haversine", "vincenty", "cheap")
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  measure <- match.arg(measure)
  
  if ( !is.numeric(distanceThreshold) )
    stop("Parameter 'distanceThreshold' must be a numeric value.")
  
  # ----- Subset locationTbl ---------------------------------------------------
  
  adjacentDistanceTbl <- table_findAdjacentDistances(locationTbl, distanceThreshold, measure)
  
  # > head(adjacentDistanceTbl)
  # # A tibble: 6 × 3
  #    row1  row2 distance
  #   <int> <int>    <dbl>
  # 1   117   800    0.334
  # 2   282   652    0.444
  # 3   250   269    0.445
  # 4   117   795    0.445
  # 5    22   910    0.445
  # 6    22   518    0.445
  
  # Base R tricks to return unique set of indices in order of separation distance
  indices <- 
    adjacentDistanceTbl[,1:2] %>%
    as.matrix() %>%
    t() %>%
    as.numeric() %>%
    unique()
  
  adjacentLocationTbl <- locationTbl[indices,]
  
  # ----- Return ---------------------------------------------------------------
  
  return(adjacentLocationTbl)
  
}
