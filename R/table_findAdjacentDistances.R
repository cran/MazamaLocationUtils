#'
#' @title Find distances between adjacent locations in a known locations table
#' 
#' @description Calculate distances between all locations within a known
#' locations table and return a tibble with the row indices and separation 
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
#' specifying desired method of geodesic distance calculation. 
#' 
#' See \code{geodist::\link[geodist:geodist]{geodist}} for details.
#' 
#' @return Tibble of row indices and distances for those locations separated by
#' less than \code{distanceThreshold} meters.
#' 
#' @examples 
#' library(MazamaLocationUtils)
#' 
#' meta <- wa_airfire_meta
#' 
#' # Any locations closer than 2 km?
#' table_findAdjacentDistances(meta, distanceThreshold = 2000)
#' 
#' # How about 4 km?
#' table_findAdjacentDistances(meta, distanceThreshold = 4000)
#' 
#' 
#' @rdname table_findAdjacentDistances
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_findAdjacentDistances <- function(
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
  
  # ----- Calculate distances --------------------------------------------------

  # NOTE:  Suppress annoying message:
  #
  # Maximum distance is > 100km. The 'cheap' measure is inaccurate over such
  # large distances, you'd likely be better using a different 'measure'.
  
  suppressMessages({

    distance <-
      geodist::geodist(
        x = cbind(
          "x" = locationTbl$longitude,
          "y" = locationTbl$latitude
        ),
        paired = FALSE,
        sequential = FALSE,
        pad = FALSE,
        measure = measure
      )
    
  })
  
  # NOTE:  distance matrix is nrow(locationTbl) X length(longitude)
  
  # ----- Check for locations that are too close -------------------------------
  
  # Get distances that are less than the given distanceThreshold
  # NOTE: the distance between a location and itself is always zero
  distanceMask <- (distance != 0) & (distance < distanceThreshold )
  
  # Select the locations that are "too close".
  adjacentIndices <- which(distanceMask, arr.ind = TRUE)
  
  if ( nrow(adjacentIndices) == 0 ) {
    
    # Return an empty tibble
    adjacentDistanceTable <- 
      dplyr::tibble(
        row1 = 1,
        row2 = 1,
        distance = as.numeric(NA)
      ) %>% dplyr::filter(
        .data$row1 == -999
      )
    
  } else {
    
    # NOTE:  If location a and b are too close, two entries will be returned:
    # NOTE:        row  col
    # NOTE:   [#,]  a    b
    # NOTE:    ...
    # NOTE:   [#,]  b    a
    #
    # NOTE:  While often the case, there is no guarantee that complementary
    # NOTE:  rows will be sequential. The next couple of lines
    # NOTE:  find the rows that have the same indices and reduce the table to
    # NOTE:  only unique pairs.
    
    sortedAdjacentIndices <- t(apply(adjacentIndices, 1, sort))
    
    # NOTE:  We have to be careful when there is only one pair so that we don't
    # NOTE:  unintentionally get simplified to a vector instead of a matrix.
    
    if ( nrow(sortedAdjacentIndices) == 2 ) {
      adjacentMatrix <- matrix(sortedAdjacentIndices[1,], nrow = 1, byrow = TRUE)
    } else {
      adjacentMatrix <- sortedAdjacentIndices[!duplicated(sortedAdjacentIndices),]
    }
    
    adjacentDistanceTable <- dplyr::tibble(
      row1 = adjacentMatrix[,1],
      row2 = adjacentMatrix[,2],
      distance = as.numeric(NA)
    )
    
    for ( i in seq_len(nrow(adjacentMatrix)) ) {
      adjacentDistanceTable$distance[i] <- 
        distance[adjacentDistanceTable$row1[i], adjacentDistanceTable$row2[i]]
    }
    
    adjacentDistanceTable <- 
      adjacentDistanceTable %>% 
      dplyr::arrange(.data$distance)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(adjacentDistanceTable)
  
}
