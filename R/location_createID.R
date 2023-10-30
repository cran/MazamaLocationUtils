#'
#' @title Create one or more unique locationIDs
#' @description A unique locationID is created for each incoming
#' \code{longitude} and \code{latitude}. 
#' 
#' See \code{geohashTools::\link[geohashTools:gh_encode]{gh_encode}} for details.
#' 
#' At \code{precision = 10}, this results in a maximum error of 0.6 meters which 
#' is more than precise enough for environmental monitoring studies making use
#' of this package.
#' 
#' An excellent way to become familiar with geohash is through the 
#' \href{https://geohash.softeng.co}{GeoHash Explorer}.
#' 
#' @note The \code{"digest"} algorithm is deprecated but provided for backwards 
#' compatibility with databases that were built using locationIDs generated
#' with this algorithm.
#' 
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param algorithm Algorithm to use -- either \code{"geohash"} or \code{"digest"}.
#' @param precision \code{precision} argument used when encoding with \code{"geohash"}.
#' 
#' @return Vector of character locationIDs.
#' 
#' @examples
#' library(MazamaLocationUtils)
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' locationID <- location_createID(lon, lat)
#' print(locationID)
#' 
#' location_createID(lon, lat, algorithm = "geohash")
#' location_createID(lon, lat, algorithm = "geohash", precision = 7)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Decimal_degrees}
#' @references \url{https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/}
#' @rdname location_createID
#' @export 
#' 
location_createID <- function(
  longitude = NULL,
  latitude = NULL,
  algorithm = c("geohash", "digest"),
  precision = 10
) {
  
  algorithm <- match.arg(algorithm)

  # TODO:  use precision here when MazamaCoreUtils provides it
  returnVal <- MazamaCoreUtils::createLocationID(longitude, latitude, algorithm)
  
  if ( algorithm == "geohash" ) {
    returnVal <- returnVal %>% stringr::str_sub(1, precision)
  }
  
  return(returnVal)
  
}