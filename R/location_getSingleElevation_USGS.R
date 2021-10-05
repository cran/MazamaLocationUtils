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
#' @description USGS APIs are used to determine the elevation associated with
#' the \code{longitude} and \code{latitude}.
#' 
#' @examples
#' \donttest{
#' library(MazamaLocationUtils)
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' location_getSingleElevation_USGS(lon, lat)
#' }
#' 
#' @references \url{https://nationalmap.gov/epqs/}
#' 
location_getSingleElevation_USGS <- function(
  longitude = NULL,
  latitude = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  

  # ----- Get USGS elevation data ----------------------------------------------
  
  # https://nationalmap.gov/epqs/pqs.php?x=-123.4&y=47.24&units=Meters&output=json
  
  # Create url
  url <- httr::parse_url("https://nationalmap.gov/epqs/pqs.php")
  
  url$query <- list(
    x= longitude,
    y = latitude,
    units = 'Meters',
    output = 'json'
  )
  
  # Get and parse the return
  r <- httr::GET(httr::build_url(url))
  if ( httr::http_error(r) ) {
    
    elevation <- as.numeric(NA)
    
    if ( verbose ) {
      warning(sprintf(
        "USGS elevation service failed for URL %s", 
        httr::build_url(url)
      ))
    }
    
  } else {
    
    returnObj <- httr::content(r)
    eq <- returnObj$USGS_Elevation_Point_Query_Service$Elevation_Query
    
    if ( !is.null(eq) ) {
      
      # See https://nationalmap.gov/epqs/
      elevation <- ifelse(eq$Elevation < -999999, 0, eq$Elevation)
      
      # TODO:  If we were being careful we would check the returned x,y
      # TODO:  to see how much they differ from the requested lon,lat
      # TODO:  Initial tests show the results to be pretty good.
      
    } else {
      
      elevation <- as.numeric(NA)
      
      if ( verbose ) {
        warning(sprintf(
          "USGS elevation service returned a NULL Elevation_Query object"
        ))
      }
      
    }
    
  }
  
  return(elevation)
  
}