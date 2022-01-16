#' @rdname location_getSingleAddress_Photon
#' @export 
#' @importFrom utils capture.output
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_detect str_sub str_subset
#' 
#' @title Get address data from the Photon API to OpenStreetMap
#' 
#' @param longitude Single longitude in decimal degrees E.
#' @param latitude Single latitude in decimal degrees N.
#' @param baseUrl Base URL for data queries.
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return List of address components.
#' 
#' @description The Photon API is used get address data associated with
#' the \code{longitude} and \code{latitude}. The following list of data
#' is returned:
#' 
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
#' 
#' The function makes an effort to convert both \code{state} and \code{country} 
#' \code{Name} into \code{Code} with codes defaulting to \code{NA}. Both 
#' \code{Name} and \code{Code} are returned so that improvements can be made in 
#' the conversion algorithm.
#' 
#' @examples
#' \donttest{
#' library(MazamaLocationUtils)
#' 
#' # Fail gracefully if any resources are not available
#' try({
#' 
#'   # Set up standard directories and spatial data
#'   spatialDataDir <- tempdir() # typically "~/Data/Spatial"
#'   mazama_initialize(spatialDataDir)
#' 
#'   # Wenatchee
#'   lon <- -120.325278
#'   lat <- 47.423333
#' 
#'   addressList <- location_getSingleAddress_Photon(lon, lat)
#'   str(addressList)
#'   
#' }, silent = FALSE)
#' }
#' 
#' @references \url{https://photon.komoot.io}
#' 
location_getSingleAddress_Photon <- function(
  longitude = NULL,
  latitude = NULL,
  baseUrl = "https://photon.komoot.io/reverse",
  verbose = TRUE
) {

  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  
  
  # ----- Get Photon address data ----------------------------------------------

  .params <- list(
    lon = longitude,
    lat = latitude
  )
    
  suppressWarnings({
    r <- httr::GET(baseUrl, query = .params)
  })

  if ( httr::http_error(r) ) {
    stop("An error has occured while querying the Photon API.")
  }
    
  fileString <- httr::content(r, 'text', encoding = 'UTF-8')
    
  if ( stringr::str_detect(fileString, "404 Not found") ) {
    stop("The Photon API returned no data for the provided longitude and latitude.")
  }
  
  photonJSON <- jsonlite::fromJSON(fileString)
  photonList <- photonJSON$features$properties
  
  # ----- Validate Photon Address Data -----------------------------------------

  # Replace "... Not Found" with NA
  photonList <- lapply(photonList, function(x) {
    if ( typeof(x) == "character" ) {
      if ( stringr::str_detect(x, "Not Found") ) {
        return(as.character(NA))
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  })
  
  # NOTE:  A careful check of the addresses returned by Photon shows them to be
  # NOTE:  incorrect by up to 6 blocks. So we will throw those away. Also, we saw
  # NOTE:  a zip in Colleville specified as "SUPER ONE FOODS". So we have to 
  # NOTE:  validate that as well.
  
  photonList$housenumber <- as.character(NA)
  photonList$street <- as.character(NA)
  
  if ( photonList$country %in% c("United States") ) {
    postcode <- 
      stringr::str_sub(photonList$postcode, 1, 5) %>%
      stringr::str_subset("^[0-9]{5}$")
    if ( length(postcode) > 0 ) {
      photonList$postcode <- postcode
    } else {
      photonList$postcode <- as.character(NA)
    }
  }
  
  # NOTE: Sometimes state contains the state code and sometimes contains the state name
  if ( nchar(photonList$state) > 2 ) {
    photonList$stateName <- photonList$state
    photonList$stateCode <- as.character(NA)
  } else {
    photonList$stateName <- as.character(NA)
    photonList$stateCode <- photonList$state
  }
  
  # ----- Create addressList ---------------------------------------------------
  
  addressList <- list(
    houseNumber = photonList$housenumber,
    street = photonList$street,
    city = photonList$city,
    stateName = photonList$stateName,
    stateCode = photonList$stateCode,
    zip = photonList$postcode,
    countryName = photonList$country,
    countryCode = photonList$countrycode
  )
  
  # countryCode 
  if ( is.na(addressList$countryCode) ) {
    suppressWarnings({
      addressList$countryCode <- 
        MazamaSpatialUtils::countryToCode(addressList$countryName)
    })
  }

  # stateCode
  if ( is.na(addressList$stateCode) ) {
    suppressWarnings({
      addressList$stateCode <- 
        MazamaSpatialUtils::stateToCode(
          stateNames = addressList$stateName,
          countryCodes = addressList$countryCode,
          dataset = "NaturalEarthAdm1"
        )
    }) 
  }
  
  # stateName
  if ( is.na(addressList$stateName) ) {
    suppressWarnings({
      addressList$stateName <- 
        MazamaSpatialUtils::US_stateCodeToName(
          stateCode = addressList$stateCode
        )
    }) 
  }

  # ----- Return ---------------------------------------------------------------
  
  return(addressList)
  
}
