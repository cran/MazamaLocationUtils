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
#'   lon <- -120.325278
#'   lat <- 47.423333
#' 
#'   location_getSingleElevation_USGS(lon, lat)
#'   
#' }, silent = FALSE)
#' }
#' 
#' @references \url{https://apps.nationalmap.gov/pqs/}
#' 
location_getSingleElevation_USGS <- function(
  longitude = NULL,
  latitude = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  

  # ----- Get USGS elevation data ----------------------------------------------
  
  # https://apps.nationalmap.gov/pqs/#/result?x=-122.2345&y=47.1234&units=Meters&format=JSON&wkid=4326&includeDate=False
  
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
  
  # Under the hood:
  
  # https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/getSamples?geometry={%27x%27:-122.234000,%27y%27:47.567000,%27spatialReference%27:{%27wkid%27:4326}}&geometryType=esriGeometryPoint&returnFirstValueOnly=true&f=json
  
  # Returns:
  
  # {"samples":[{"location":{"x":-122.23399999999999,"y":47.234000000000002,"spatialReference":{"wkid":4326,"latestWkid":4326}},"locationId":0,"value":"16.342819214","rasterId":76313,"resolution":1}]}
  
  url <- sprintf(
    "https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/getSamples?geometry={'x':%f,'y':%f,'spatialReference':{'wkid':4326}}&geometryType=esriGeometryPoint&returnFirstValueOnly=true&f=json",
    longitude, latitude
  )
  
  r <- httr::GET(url)
  
  if ( httr::http_error(r) ) {
    
    elevation <- as.numeric(NA)
    
    if ( verbose ) {
      warning(sprintf(
        "USGS elevation service failed for URL %s", 
        url
      ))
    }
    
  } else {
    
    returnObj <- httr::content(r)
    sample <- returnObj$samples[[1]]
    
    if ( !is.null(sample) ) {
      
      elevation <- as.numeric(sample$value)
      
      # NOTE:  If we were being extra careful we would check the returned x,y
      # NOTE:  to see how much they differ from the requested lon,lat.
      # NOTE:  Initial tests show the results to be pretty good.
      
    } else {
      
      elevation <- as.numeric(NA)
      
      if ( verbose ) {
        warning(sprintf(
          "USGS elevation service returned a NULL Elevation_Query object"
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