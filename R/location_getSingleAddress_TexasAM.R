#' @rdname location_getSingleAddress_TexasAM
#' @export 
#' 
#' @title Get an address from the Texas A&M reverse geocoding service
#' 
#' @param longitude Single longitude in decimal degrees E.
#' @param latitude Single latitude in decimal degrees N.
#' @param verbose Logical controlling the generation of progress messages.
#' @param apiKey Texas A&M Geocoding requires an API key. The first 2500 requests
#' are free.
#' 
#' @return Numeric elevation value.
#' 
#' @description Texas A&M APIs are used to determine the address associated with
#' the \code{longitude} and \code{latitude}.
#' 
#' @examples
#' \dontrun{
#' library(MazamaLocationUtils)
#' 
#' # Fail gracefully if any resources are not available
#' try({
#' 
#'   # Wenatchee
#'   longitude <- -122.47
#'   latitude <- 47.47
#'   apiKey <- YOUR_PERSONAL_API_KEY
#' 
#'   location_getSingleAddress_TexasAM(longitude, latitude, apiKey)
#'   
#' }, silent = FALSE)
#' }
#' 
#' @references \url{https://geoservices.tamu.edu/Services/ReverseGeocoding/WebService/v04_01/HTTP.aspx}
#' 
location_getSingleAddress_TexasAM <- function(
  longitude = NULL,
  latitude = NULL,
  apiKey = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(verbose)
  MazamaCoreUtils::stopIfNull(apiKey)
  
  validateLonLat(longitude, latitude)  
  
  # ----- Get Texas A&M address data -------------------------------------------
  
  # Create url
  url <- httr::parse_url("https://geoservices.tamu.edu/Services/ReverseGeocoding/WebService/v04_01/HTTP/default.aspx")
  
  url$query <- list(
    apikey = apiKey,
    version = "4.01",
    lat = latitude,
    lon = longitude,
    format = "json"
  )
  
  
  result <- try({
    # Get and parse the return
    r <- httr::GET(httr::build_url(url))
  })
  
  if ( "try-error" %in% result )
    stop(geterrmessage())
  
  if ( httr::http_error(r) ) {
    
    addressList <- list(
      houseNumber = as.character(NA),
      street = as.character(NA),
      city = as.character(NA),
      stateCode = as.character(NA),
      postalCode = as.character(NA),
      countryCode = as.character(NA),
      countryName = as.character(NA)
    ) 
    
    if ( verbose ) {
      warning(sprintf(
        "Texas A&M reverse geocoding service failed for URL %s", 
        httr::build_url(url)
      ))
    }
    
  } else {
    
    # Get the json return
    jsonString <- httr::content(r)
    
    # Parse the json
    returnObj <- as.list(jsonlite::fromJSON(jsonString,
                                            simplifyVector = TRUE,
                                            simplifyDataFrame = FALSE,
                                            simplifyMatrix = FALSE,
                                            flatten = FALSE))
    
    # Extract desired information
    locationInfo <- returnObj$StreetAddresses[[1]] 
    
    address <- 
      locationInfo$StreetAddress %>% 
      stringr::str_trim()
    
    # Parse the address string into houseNumber and street
    if ( locationInfo$StreetAddress == "" ) {
      houseNumber <- as.character(NA)
      street <- as.character(NA)
    } else {
      parts <- stringr::str_match(address, "(\\d+) (.+)")
      houseNumber <- parts[,2]
      street <- parts[,3]
    }
    
    if ( locationInfo$City == "" ) {
      locationInfo$City <- as.character(NA)
    }
    
    # Assemble the addressList
    addressList <- list(
      houseNumber = as.character(houseNumber),
      street = as.character(street),
      city = as.character(locationInfo$City),
      stateCode = toupper(locationInfo$State),
      postalCode = as.character(locationInfo$Zip),
      countryCode = "US",
      countryName = "United States of America" # Texas A&M ONLY works in US
    )
    
  }
  
  # Fill in the stateName
  suppressWarnings({
    addressList$stateName <- 
      MazamaSpatialUtils::codeToState(
        stateCodes = addressList$stateCode,
        countryCodes = addressList$countryCode,
        dataset = "NaturalEarthAdm1"
      )
  })
  
  # ----- Return ---------------------------------------------------------------
  
  return(addressList)
  
}