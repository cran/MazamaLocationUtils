
#' @title Get address data from the Photon API to OpenStreetMap
#' @description The Photon API is used get address data associated with
#' the \code{longitude} and \code{latitude}. The following list of data
#' is returned:
#' \itemize{
#' \item{\code{houseNumber}}
#' \item{\code{street}}
#' \item{\code{city}}
#' \item{\code{stateCode}}
#' \item{\code{stateName}}
#' \item{\code{zip}}
#' \item{\code{countryCode}}
#' \item{\code{countryName}}
#' }
#' The function makes an effort to convert both \code{state} and \code{country} 
#' \code{Name} into \code{Code} with codes defaulting to \code{NA}. Both 
#' \code{Name} and \code{Code} are returned so that improvements can be made in 
#' the conversion algorithm.
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return List of address components.
#' @examples 
#' \donttest{
#' # Set up standard directories and spatial data
#' spatialDataDir <- tempdir() # typically "~/Data/Spatial"
#' mazama_initialize(spatialDataDir)
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' addressList <- location_getSingleAddress_Photon(lon, lat)
#' str(addressList)
#' }
#' @references \url{http://photon.komoot.de}
#' @rdname location_getSingleAddress_Photon
#' @export 
#' @importFrom utils capture.output
#' @importFrom revgeo revgeo
#' @importFrom stringr str_detect str_sub str_subset
#' 
location_getSingleAddress_Photon <- function(
  longitude = NULL,
  latitude = NULL,
  verbose = TRUE
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  
  
  # ----- Get Photon address data ----------------------------------------------
  
  result <- try({
    
    # Capture output to deal with revgeo print() statements
    outputString <- utils::capture.output({
      revgeoList <- revgeo::revgeo(
        longitude = longitude, 
        latitude = latitude, 
        provider = NULL, # default to Photon
        output = "hash"
      )
    })
    if ( verbose )
      message(outputString)
    
  })
  
  if ( "try-error" %in% result )
    stop(geterrmessage())
  
  # Replace "... Not Found" with NA
  revgeoList <- lapply(revgeoList, function(x) {
    if ( stringr::str_detect(x, "Not Found") ) {
      return(as.character(NA))
    } else {
      return(x)
    }
  })
  
  # NOTE:  A careful check of the addresses returned by Photon shows them to
  # NOTE:  incorrect by up to 6 blocks. So we will throw those away. Also, we
  # NOTE:  a zip in Colleville specified as "SUPER ONE FOODS". So we have to 
  # NOTE:  validate that as well.
  
  revgeoList$housenumber <- as.character(NA)
  revgeoList$street <- as.character(NA)
  
  if ( revgeoList$country %in% c("United States of America") ) {
    zip <- 
      stringr::str_sub(revgeoList$zip, 1, 5) %>%
      stringr::str_subset("^[0-9]{5}$")
    if ( length(zip) > 0 ) {
      revgeoList$zip <- zip
    } else {
      revgeoList$zip <- as.character(NA)
    }
  }
  
  # ----- Create addressList ---------------------------------------------------
  
  addressList <- list(
    houseNumber = revgeoList$housenumber,
    street = revgeoList$street,
    city = revgeoList$city,
    stateName = revgeoList$state,
    zip = revgeoList$zip,
    countryName = revgeoList$country
  )
  
  # countryCode 
  suppressWarnings({
    addressList$countryCode <- 
      MazamaSpatialUtils::countryToCode(addressList$countryName)
  })
  
  # stateCode
  suppressWarnings({
    addressList$stateCode <- 
      MazamaSpatialUtils::stateToCode(
        stateNames = addressList$stateName,
        countryCodes = addressList$countryCode,
        dataset = "NaturalEarthAdm1"
      )
  })
  
  # ----- Return ---------------------------------------------------------------
  
  return(addressList)
  
}