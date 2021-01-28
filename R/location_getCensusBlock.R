
#' @title Get census block data from the FCC API
#' @description The FCC Block API is used get census block, county, and state FIPS associated with
#' the \code{longitude} and \code{latitude}. The following list of data
#' is returned:
#' \itemize{
#' \item{\code{stateCode}}
#' \item{\code{county}}
#' \item{\code{censusBlock}}
#' }
#' The data from this function should be considered to be the gold standard for state and county.
#' i.e. this information could and should be used to override information we get elsewhere.
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return List of census block/county/state data.
#' @examples 
#' \donttest{
#' library(MazamaLocationUtils)
#' 
#' lon <- -77.51
#' lat <- 38.26
#' 
#' censusList <- location_getCensusBlock(lon, lat)
#' str(censusList)
#' }
#' @references \url{https://geo.fcc.gov/api/census/#!/block/get_block_find}
#' @rdname location_getCensusBlock
#' @export 
#' @importFrom utils capture.output
#' @importFrom stringr str_detect str_sub str_subset
#' 
location_getCensusBlock <- function(
  longitude = NULL,
  latitude = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  
  
  # ----- Get FCC API data ----------------------------------------------
  
  # https://geo.fcc.gov/api/census/block/find?latitude=38.26&longitude=-77.51&showall=false&format=json
  
  # Create url
  
  url <- httr::parse_url("https://geo.fcc.gov/api/census/block/find")
  
  url$query <- list(
    latitude=latitude, 
    longitude=longitude, 
    showall=FALSE,
    format="json"
  )
  
  # Get and parse the return
  
  r <- httr::GET(httr::build_url(url))
  if ( httr::http_error(r) ) {
    
    censusBlock <- list()
    
    if ( verbose ) {
      warning(sprintf(
        "FCC Census Block service failed for URL %s", 
        httr::build_url(url)
      ))
    }
    
  } else {
    
    returnObj <- httr::content(r)
    censusList <- list(
      stateCode = returnObj$State$code,
      county = returnObj$County$name,
      censusBlock = returnObj$Block$FIPS
    )
    
    if (is.null(censusList$censusBlock) ) {
      
      if ( verbose ) {
        warning(sprintf(
          "FCC Census Block service returned NULL values."
        ))
      }
      
    }
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(censusList)
  
}
