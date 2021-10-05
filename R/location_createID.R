
#' @title Create one or more unique locationIDs
#' @description A unique locationID is created for each incoming
#' \code{longitude} and \code{latitude}. The following code is used to generate
#' each locationID. See the references for details.
#' 
#' \preformatted{
#' # Retain accuracy up to ~.1m
#' locationString <- paste0(
#'   sprintf("\%.7f", longitude),
#'   "_",
#'   sprintf("\%.7f", latitude)
#' )
#'   
#' # Avoid collisions until billions of records
#' locationID <- digest::digest(locationString, algo = "xxhash64")  
#' }
#' 
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @return Vector of character locationIDs.
#' @examples
#' library(MazamaLocationUtils)
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' locationID <- location_createID(lon, lat)
#' @references \url{https://en.wikipedia.org/wiki/Decimal_degrees}
#' @references \url{https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/}
#' @rdname location_createID
#' @export 
#' 
location_createID <- function(
  longitude = NULL,
  latitude = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonsLats(longitude, latitude)
  
  # ----- Create location hash -------------------------------------------------
  
  # meters per decimal degree:
  #  https://en.wikipedia.org/wiki/Decimal_degrees
  
  # Retain accuracy up to ~.1m
  locationString <- paste0(
    sprintf("%.7f", longitude),
    "_",
    sprintf("%.7f", latitude)
  )
  
  # Explanation of collision frequency:
  #   https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/
  #
  # > a hash function with range of size N can hash on the order of √N values 
  # > before running into collisions.
  #
  # A 32 bit hash will run into collisions at (2^32)^0.5 = 65,536
  # A 64 bit hash will run into collisions at (2^64)^0.5 = 4,294,967,296
  #
  # One can imagine a table with 60K known locations so it looks like a 32 bit 
  # hash is not quite safe enough.
  
  # Use base::() mnapply to vectorise digest::digest()
  locationID <- mapply( 
    function(x) { digest::digest(x, algo = "xxhash64") }, 
    locationString,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationID)
  
}