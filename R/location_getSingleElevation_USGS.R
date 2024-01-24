#' @rdname location_getSingleElevation_USGS
#' @export 
#' 
#' @title Get elevation data from a USGS web service
#' 
#' @param longitude Single longitude in decimal degrees E.
#' @param latitude Single latitude in decimal degrees N.
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return Numeric elevation value.
#' 
#' @description USGS APIs are used to determine the elevation in meters 
#' associated with the \code{longitude} and \code{latitude}.
#' 
#' _Note: The conversion factor for meters to feet is 3.28084._
#' 
#' @examples
#' \donttest{
#' library(MazamaLocationUtils)
#' 
#' # Fail gracefully if any resources are not available
#' try({
#' 
#'   # Wenatchee
#'   longitude <- -120.325278
#'   latitude <- 47.423333
#' 
#'   location_getSingleElevation_USGS(longitude, latitude)
#'   
#' }, silent = FALSE)
#' }
#' 
#' @references \url{https://apps.nationalmap.gov/epqs/}
#' 
location_getSingleElevation_USGS <- function(
  longitude = NULL,
  latitude = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  

  # ----- Get USGS elevation data ----------------------------------------------
  
  # https://epqs.nationalmap.gov/v1/json?x=-122.5&y=47.5&wkid=4326&units=Meters&includeDate=false
  
  # {
  #   "location": {
  #     "x": -122.2345,
  #     "y": 47.1234,
  #     "spatialReference": {
  #       "wkid": 4326,
  #       "latestWkid": 4326
  #     }
  #   },
  #   "locationId": 0,
  #   "value": "42.277534485",
  #   "rasterId": 56925,
  #   "resolution": 1
  # }
  
  url <- sprintf(
    "https://epqs.nationalmap.gov/v1/json?x=%f&y=%f&wkid=4326&units=Meters&includeDate=false",
    longitude, latitude
  )
  
  r <- httr::GET(url)
  
  # Default to NA unless we get a result
  elevation <- as.numeric(NA)
  
  if ( httr::http_error(r) ) {
    
    if ( verbose ) {
      warning(sprintf(
        "USGS elevation service failed for URL %s", 
        url
      ))
    }
    
  } else {
    
    result <- try({
      
      returnObj <- httr::content(r)
      elevation <- as.numeric(returnObj$value)
      
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      if ( verbose ) {
        warning(sprintf(
          "USGS elevation service returned an error for this request:\n  %s",
          url
        ))
      }
    }
    
  }
  
  # This page says the vertical accuracy has a RMSE of .53 meters
  # https://www.usgs.gov/faqs/what-vertical-accuracy-3d-elevation-program-3dep-dems
  return(round(elevation, 1))
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(MazamaCoreUtils)
  
  longitude <- -112.234
  latitude <- 57.234
  verbose <- FALSE
  
  
  location_getSingleElevation_USGS(longitude, latitude, verbose)
  
  
  
}