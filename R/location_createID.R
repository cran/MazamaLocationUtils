#'
#' @title Create one or more unique locationIDs
#' @description A unique locationID is created for each incoming
#' \code{longitude} and \code{latitude}. 
#' 
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:createLocationID]{createLocationID}} for details.
#'
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param algorithm Algorithm to use -- either \code{"digest"} or \code{"geohash"}.
#' @return Vector of character locationIDs.
#' @examples
#' library(MazamaLocationUtils)
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' locationID <- location_createID(lon, lat)
#' print(locationID)
#' 
#' geohashID <- location_createID(lon, lat, algorithm = "geohash")
#' print(geohashID)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Decimal_degrees}
#' @references \url{https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/}
#' @rdname location_createID
#' @export 
#' 
location_createID <- function(
  longitude = NULL,
  latitude = NULL,
  algorithm = c("digest", "geohash")
) {

  returnVal <- MazamaCoreUtils::createLocationID(longitude, latitude, algorithm)
  return(returnVal)
  
}