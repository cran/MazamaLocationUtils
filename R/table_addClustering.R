#' @export
#'
#' @title Add clustering information to a dataframe
#'
#' @param tbl Tibble with geolocation information (\emph{e.g.}.
#' @param clusterDiameter Diameter in meters used to determine the number of
#' clusters (see description).
#' @param lonVar Name of longitude variable in the incoming tibble.
#' @param latVar Name of the latitude variable in the incoming tibble.
#' @param maxClusters Maximum number of clusters to try.
#'
#' @description Clustering is used to identify unique deployments of a
#' sensor in an environmental monitoring field study.
#'
#' Sensors will be moved around from time to time, sometimes across the country
#' and sometimes across the street.  We would like to assign unique identifiers
#' to each new "deployment" but not when the sensor is moved a short distance.
#'
#' We use clustering to find an appropriate number of unique "deployments".
#' The sensitivity of this algorithm can be adjused with the clusterDiameter argument.
#'
#' Standard \code{kmeans} clustering does not work well when clusters can have widely
#' differing numbers of members. A much better result is acheived with
#' the Partitioning Around Medoids method available in \code{cluster::pam()}.
#'
#' The value of \code{clusterRadius} is compared with the output of
#' \code{cluster::pam(...)$clusinfo[,'av_diss']} to determine the number of clusters.
#' 
#' @note The \code{table_addClustering()} function implements two-stage clustering
#' using \link{clusterByDistance}. If the first attempt at clustering produces
#' clustered locations that are still too close to eachother, another round
#' of clustering is performed using the results of the previous attempt. This
#' two-stage approach seems to work well in. practice. 
#'
#' @return Input tibble with additional columns: \code{clusterLon, clusterLat}.
#'
#' @references \href{https://working-with-data.mazamascience.com/2021/07/15/when-k-means-clustering-fails/}{When k-means clustering fails}
#' 
#' @seealso \link{clusterByDistance}
#' 
#' @examples
#' library(MazamaLocationUtils)
#'
#' # Fremont, Seattle 47.6504, -122.3509
#' # Magnolia, Seattle 47.6403, -122.3997
#' # Downtown Seattle 47.6055, -122.3370
#' 
#' fremont_x <- jitter(rep(-122.3509, 10), .0005)
#' fremont_y <- jitter(rep(47.6504, 10), .0005)
#' 
#' magnolia_x <- jitter(rep(-122.3997, 8), .0005)
#' magnolia_y <- jitter(rep(47.6403, 8), .0005)
#' 
#' downtown_x <- jitter(rep(-122.3370, 3), .0005)
#' downtown_y <- jitter(rep(47.6055, 3), .0005)
#' 
#' # Apply clustering
#' tbl <-
#'   dplyr::tibble(
#'     longitude = c(fremont_x, magnolia_x, downtown_x),
#'     latitude = c(fremont_y, magnolia_y, downtown_y)
#'   ) %>%
#'   table_addClustering(
#'     clusterDiameter = 1000
#'   )
#' 
#' plot(tbl$longitude, tbl$latitude, pch = tbl$clusterID)


table_addClustering <- function(
  tbl,
  clusterDiameter = 1000,
  lonVar = "longitude",
  latVar = "latitude",
  maxClusters = 50
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(tbl)
  MazamaCoreUtils::stopIfNull(clusterDiameter)
  MazamaCoreUtils::stopIfNull(lonVar)
  MazamaCoreUtils::stopIfNull(latVar)
  MazamaCoreUtils::stopIfNull(maxClusters)
  
  # Sanity check -- row count
  if ( nrow(tbl) == 0 ) {
    stop(paste0("Unable to perform clustering: 'tbl' is empty"))
  }

  # Sanity check -- names
  if ( !lonVar %in% names(tbl) ) {
    stop(paste0("Longitudes could not be found.  Did you specify the lonVar argument?"))
  }
  if ( !latVar %in% names(tbl) ) {
    stop(paste0("Latitudes could not be found.  Did you specify the latVar argument?"))
  }

  # If we only have a single row, return immediately
  # NOTE:  Use as.numeric() to remove any names associated with these vectors
  if ( nrow(tbl) == 1 ) {
    tbl$clusterLon <- as.numeric(tbl[[lonVar]][1])
    tbl$clusterLat <- as.numeric(tbl[[latVar]][1])
    tbl$clusterID <- 1
    return(tbl)
  }

  # ----- Cluster by distance --------------------------------------------------

  tbl <-
    clusterByDistance(
      tbl,
      clusterDiameter = clusterDiameter,
      lonVar = lonVar,
      latVar = latVar,
      maxClusters = maxClusters
    )

  # ----- Verify clustering ----------------------------------------------------

  # Only use one location record per clusterID
  distinctTbl <-
    tbl %>%
    dplyr::distinct(.data$clusterID, .keep_all = TRUE) %>%
    dplyr::mutate(
      longitude = .data$clusterLon,
      latitude = .data$clusterLat
    ) %>%
    dplyr::select(c("longitude", "latitude"))

  # Check if any of the clustered locations are too close
  adjacentDistances <-
    distinctTbl %>%
    MazamaLocationUtils::table_findAdjacentDistances(
      distanceThreshold = clusterDiameter
    )

  # ----- Fix clustering -------------------------------------------------------

  if ( nrow(adjacentDistances) > 0 ) {

    # NOTE:  Clearly, clustering has failed. At this point we just cluster the
    # NOTE:  distinct locations. Having just a few points should radically
    # NOTE:  improve the reliability of cluster::pam().

    # Cluster the distinct locations
    distinctTbl <-
      clusterByDistance(
        distinctTbl,
        clusterDiameter = clusterDiameter,
        lonVar = "longitude",                # the renamed clusterLon
        latVar = "latitude",                 # the renamed clusterLat
        maxClusters = maxClusters
      )

    clusterCount <- max(as.numeric(distinctTbl$clusterID), na.rm = TRUE)

    # Now use this clusterCount for full clustering -- see clusterByDistance()

    if ( nrow(tbl) < 2000 ) {
      clusterObj <- cluster::pam(tbl[,c(lonVar,latVar)], clusterCount)
    } else {
      clusterObj <- cluster::clara(tbl[,c(lonVar,latVar)], clusterCount, samples = 50)
    }

    tbl$clusterLon <- as.numeric(clusterObj$medoids[,lonVar][clusterObj$clustering])
    tbl$clusterLat <- as.numeric(clusterObj$medoids[,latVar][clusterObj$clustering])
    tbl$clusterID <- as.character(clusterObj$clustering)

  }

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}

# ===== DEBUG ==================================================================

if ( FALSE ) {


}
