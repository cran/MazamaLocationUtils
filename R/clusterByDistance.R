#' @export
#'
#' @title Add distance-clustering information to a dataframe
#'
#' @param tbl Tibble with geolocation information.
#' @param clusterDiameter Diameter in meters used to determine the number of
#' clusters (see description).
#' @param lonVar Name of longitude variable in the incoming tibble.
#' @param latVar Name of the latitude variable in the incoming tibble.
#' @param maxClusters Maximum number of clusters to try.
#'
#' @description Distance clustering is used to identify unique deployments of a
#' sensor in an environmental monitoring field study. GPS-reported locations can
#' be jittery and result in a sensor self-reporting from a cluster of nearby locations.
#' Clustering helps resolve this by assigning a single location to the cluster.
#'
#' Standard \code{kmeans} clustering does not work well when clusters can have 
#' widely differing numbers of members. A much better result is acheived with
#' the Partitioning Around Medoids method available in \code{cluster::pam()}.
#'
#' The value of \code{clusterDiameter} is compared with the output of
#' \code{cluster::pam(...)$clusinfo[,'av_diss']} to determine the number of clusters.
#' 
#' @note In most applications, the \link{table_addClustering} function should be 
#' used as it implements two-stage clustering using \code{clusterbyDistance()}.
#'
#' @return Input tibble with additional columns: \code{clusterLon, clusterLat, clusterID}.
#'
#' @references \href{https://working-with-data.mazamascience.com/2021/07/15/when-k-means-clustering-fails/}{When k-means clustering fails}
#' 
#' @seealso \link{table_removeRecord}
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
#'   clusterByDistance(
#'     clusterDiameter = 1000
#'   )
#' 
#' plot(tbl$longitude, tbl$latitude, pch = tbl$clusterID)

clusterByDistance <- function(
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

  # ----- Clustering -----------------------------------------------------------

  # NOTE:  A monitor will be moved around from time to time, sometimes across the country
  # NOTE:  and sometimes across the street.  We need to assign unique identifiers to each
  # NOTE:  new "deployment" but not when the monitor is moved only a short distance.
  # NOTE:
  # NOTE:  We use clustering to find an appropriate number of unique "deployments".
  # NOTE:  The sensitivity of this algorithm can be adjused with the clusterDiameter argument.
  # NOTE:
  # NOTE:  Standard kmeans clustering does not work well when clusters can have widely
  # NOTE:  differing numbers of members. A much better result is acheived with
  # NOTE:  the Partitioning Around Medoids method available in cluster::pam().
  # NOTE:
  # NOTE:  Try the following example:
  # NOTE:    x <- y <- c(rep(0,3), rep(1,3), rep(10,20), rep(11,20), rep(100,50), rep(101,50))
  # NOTE:    m <- cbind(x,y)
  # NOTE:    layout(matrix(seq(2)))
  # NOTE:    plot(x, y, pch = as.character( stats::kmeans(m,3)$cluster ))
  # NOTE:    plot(x, y, pch = as.character( cluster::pam(m,3)$cluster ))
  # NOTE:    plot(x, y, pch = as.character( cluster::clara(m,3)$cluster ))
  # NOTE:
  # NOTE:  Run the plots a few times and you will see that kmeans clustering sometimes
  # NOTE:  gets it wrong.

  # NOTE:  We need to use cluster::clara when we get above ~2K points.
  # NOTE:  For this reason we need to use clusinfo[,'max_diss'] instead
  # NOTE:  of clusinfo[,'diameter'] as the latter is only provided by
  # NOTE:  cluster::pam and not cluster::clara.
  # NOTE:  (Is there really any difference between 'max_diss' and 'distance'?)

  # Perform clustering
  for ( clusterCount in 1:maxClusters ) {
    
    if ( nrow(tbl) <= clusterCount ) {
      # NOTE:  k = positive integer specifying the number of clusters, less than the number of observations.
      clusterObj <- cluster::pam(tbl[,c(lonVar,latVar)], nrow(tbl) - 1)
    } else if ( nrow(tbl) < 2000 ) {
      clusterObj <- cluster::pam(tbl[,c(lonVar,latVar)], clusterCount)
    } else {
      clusterObj <- cluster::clara(tbl[,c(lonVar,latVar)], clusterCount, samples = 50)
    }
    clusterLats <- clusterObj$medoids[,latVar]
    diameters <- 2 * clusterObj$clusinfo[,'max_diss'] # decimal degrees
    # NOTE:  We don't know whether distance is pure NS, EW or some combination
    # NOTE:  so we can't accurately convert to meters. We approximate by
    # NOTE:  assuming a 50-50 split and using 111,320 meters/degree at the equator.
    radianClusterLats <- clusterLats * pi/180
    meters <- diameters * (1 + cos(radianClusterLats))/2 * 111320
    if ( max(meters) < clusterDiameter ) break
    
  }
  
  # NOTE:  If we only had a very small table with widely separate locations,
  # NOTE:  we can end up never satisfying max(meters) < clusterDiameter. In this
  # NOTE:  case, every location represents a separate cluster
  if ( nrow(tbl) <= clusterCount ) {
    for ( i in 1:seq_len(nrow(tbl)) ) {
      tbl$clusterLon <- as.numeric(tbl[[lonVar]][i])
      tbl$clusterLat <- as.numeric(tbl[[latVar]][i])
      tbl$clusterID <- i
    }
    return(tbl)
  }

  # Create the vector of deployment identifiers
  if ( nrow(tbl) < 2000 ) {
    clusterObj <- cluster::pam(tbl[,c(lonVar,latVar)], clusterCount)
  } else {
    clusterObj <- cluster::clara(tbl[,c(lonVar,latVar)], clusterCount, samples = 50)
  }

  # Add cluster lons and lats to the tibble
  # NOTE:  Use as.numeric() to remove any names associated with these vectors
  tbl$clusterLon <- as.numeric(clusterObj$medoids[,lonVar][clusterObj$clustering])
  tbl$clusterLat <- as.numeric(clusterObj$medoids[,latVar][clusterObj$clustering])
  tbl$clusterID <- as.character(clusterObj$clustering)

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}

# ===== DEBUG ==================================================================

if ( FALSE ) {

  library(MazamaCoreUtils)
  logger.setup()
  logger.setLevel(TRACE)

  library(AirMonitorIngest)

  tbl <-
    wrcc_downloadData(20150701, 20150930, unitID = 'SM16') %>%
    wrcc_parseData() %>%
    wrcc_qualityControl()

  clusterDiameter = 1000
  lonVar = "GPSLon"
  latVar = "GPSLat"
  maxClusters = 50




  newTbl <- clusterByDistance(
    tbl,
    clusterDiameter = clusterDiameter,
    lonVar = lonVar,
    latVar = latVar,
    maxClusters = maxClusters
  )


}
