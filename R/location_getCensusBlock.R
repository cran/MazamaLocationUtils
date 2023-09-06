
#' @title Get census block data from the FCC API
#' @description The FCC Block API is used get census block, county, and state FIPS associated with
#' the \code{longitude} and \code{latitude}. The following list of data
#' is returned:
#' \itemize{
#' \item{\code{stateCode}}
#' \item{\code{countyName}}
#' \item{\code{censusBlock}}
#' }
#' The data from this function should be considered to be the gold standard for state and county.
#' i.e. this information could and should be used to override information we get elsewhere.
#' @param longitude Single longitude in decimal degrees E.
#' @param latitude Single latitude in decimal degrees N.
#' @param censusYear Year the census was taken.
#' @param verbose Logical controlling the generation of progress messages.
#' @return List of census block/county/state data.
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
#'   censusList <- location_getCensusBlock(lon, lat)
#'   str(censusList)
#'   
#' }, silent = FALSE)
#' }
#' @references \url{https://geo.fcc.gov/api/census/}
#' @rdname location_getCensusBlock
#' @export 
#' @importFrom utils capture.output
#' @importFrom stringr str_detect str_sub str_subset
#' 
location_getCensusBlock <- function(
  longitude = NULL,
  latitude = NULL,
  censusYear = 2010,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  
  
  # ----- Get FCC API data ----------------------------------------------
  
  # https://geo.fcc.gov/api/census/block/find?latitude=47.423333&longitude=-120.325278&censusYear=2010&showall=false&format=json
  
  # Create url
  
  url <- httr::parse_url("https://geo.fcc.gov/api/census/block/find")
  
  url$query <- list(
    latitude = latitude, 
    longitude = longitude, 
    censusYear = censusYear,
    showall = "False",
    format = "json"
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
      stateCode = toupper(returnObj$State$code),
      countyName = as.character(returnObj$County$name),
      censusBlock = as.character(returnObj$Block$FIPS)
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
