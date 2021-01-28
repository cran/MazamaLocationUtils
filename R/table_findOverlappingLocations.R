#'
#' @title Finds overlapping locations in a known locations table.
#' 
#' @description Calculates distances between all locations within a known
#' locations table and returns a tibble with the row indices and separation 
#' distances of those records with overlapping locations.
#' 
#' It is useful when working with new metadata tables to identify overlapping
#' locations early on so that decisions can be made about the apporpriateness
#' of the specified \code{radius}.
#' 
#' @param tbl Tibble with \code{longitude} and \code{latitude} columns.
#' @param radius Radius in meters.
#' 
#' @return Tibble of row indices and distances for those locations which overlap. 
#' 
#' @examples 
#' library(MazamaLocationUtils)
#' 
#' meta <- wa_airfire_meta
#' 
#' # Anything locations closer than 2 km? (diameter = 2*radius)
#' table_findOverlappingLocations(meta, radius = 1000)
#' 
#' # How about 4 km?
#' table_findOverlappingLocations(meta, radius = 2000)
#' 
#' # Let's look at those locations
#' 
#' tooCloseTbl <- table_findOverlappingLocations(meta, radius = 2000)
#' 
#' for ( i in seq_len(nrow(tooCloseTbl)) ) {
#'   rows <- as.numeric(tooCloseTbl[i, 1:2])
#'   cat(sprintf("\n%5.1f meters apart:\n", tooCloseTbl$distance[i]))
#'   print(meta[rows, c('longitude', 'latitude', 'siteName')])
#' }
#' 
#' 
#' @rdname table_findOverlappingLocations
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_findOverlappingLocations <- function(
  tbl = NULL,
  radius = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(tbl)
  MazamaCoreUtils::stopIfNull(radius)
  
  if ( !"data.frame" %in% class(tbl) )
    stop("Parameter 'tbl' is not of class \"data.frame\".")
  
  if ( !"longitude" %in% names(tbl) )
    stop("Parameter 'tbl' does not have a 'longitude' column.")
  
  if ( !"latitude" %in% names(tbl) )
    stop("Parameter 'tbl' does not have a 'latitude' column.")
  
  if ( !is.numeric(radius) )
    stop("Parameter 'radius' must be a numeric value.")
  
  diameter <- 2 * radius
  
  # ----- Check for locations that are too close -------------------------------
  
  # Calculate distances between each location
  distances <- geodist::geodist(tbl, measure = "geodesic")
  
  # Get distances that are less than the given diameter
  # NOTE: the distance between a location and itself is always zero
  distancesLessThanR <- (distances != 0) & (distances < diameter )
  
  # Select the locations that are "too close".
  tooClose <- which(distancesLessThanR > 0, arr.ind = TRUE)
  
  if ( nrow(tooClose) == 0 ) {
    
    # Return an empty tibble
    tooCloseTbl <- 
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
    # NOTE:  rows will be adjacent to eachother. The next couple of lines
    # NOTE:  find the rows that have the same indices and reduce the table to
    # NOTE:  only unique pairs.
    
    sortedMatrix <- t(apply(tooClose, 1, sort))
    tooClose <- sortedMatrix[!duplicated(sortedMatrix),]
    
    tooCloseTbl <- dplyr::tibble(
      row1 = tooClose[,1],
      row2 = tooClose[,2],
      distance = as.numeric(NA)
    )
    
    for ( i in seq_len(nrow(tooClose)) ) {
      tooCloseTbl$distance[i] <- 
        distances[tooCloseTbl$row1[i], tooCloseTbl$row2[i]]
    }
    
    tooCloseTbl <- tooCloseTbl %>% dplyr::arrange(.data$distance)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(tooCloseTbl)
  
}
