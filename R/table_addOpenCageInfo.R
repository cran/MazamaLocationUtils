#' @export 
#' 
#' @title Add address fields to a known location table
#' 
#' @description The OpenCage reverse geocoding service is used to update an 
#' existing table. Updated columns include:
#' 
#' \itemize{
#' \item{countryCode}
#' \item{stateCode}
#' \item{countyName}
#' \item{timezone}
#' \item{houseNumber}
#' \item{street}
#' \item{city}
#' \item{zip}
#' \item{address}
#' }
#' 
#' When \code{replaceExisting = TRUE}, all existing address fields are discarded
#' in favor of the OpenCage versions. To only fill in missing values in
#' \code{locationTbl}, use \code{replaceExisting = FALSE}.
#' 
#' The OpenCage service returns a large number of fields, some of which may be
#' useful. To add all OpenCage fields to a location table, use 
#' \code{retainOpenCage = TRUE}. This will append 78+ fields of information,
#' each each named with a prefix of \code{"opencage_"}.
#' 
#' @note The OpenCage service requires an API key which can be obtained from 
#' their web site. This API key must be set as an environment variable with:
#' 
#' \preformatted{
#' Sys.setenv("OPENCAGE_KEY" = "<your api key>")
#' }
#' 
#' Parameters are set for use at the OpenCage "free trial" level which allows
#' for 1 request/sec and a maximum of 2500 requests per day.
#' 
#' Because of the 1 request/sec default, it is recommended that
#' \code{table_addOpenCageInfo()} only be used in an interactive session when 
#' updating a table with a large number of records.
#' 
#' @param locationTbl Tibble of known locations.
#' @param replaceExisting Logical specifying whether to replace existing data
#' with data obtained from OpenCage.
#' @param retainOpenCage Logical specifying whether to retain all fields obtained
#' from OpenCage, each named with a prefix of \code{opencage_}.
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return Tibble of "known locations" enhanced with information from the 
#' OpenCage reverse geocoding service.
#' 
#' @references \url{https://opencagedata.com}
#' 
#' @examples
#' \donttest{
#' library(MazamaLocationUtils)
#' 
#' # Fail gracefully if any resources are not available
#' try({
#' 
#'   myTbl <- id_monitors_500[1:3,]
#'   myTbl$countryCode[1] <- NA
#'   myTbl$countryCode[2] <- "WRONG"
#'   myTbl$countyName[3] <- "WRONG"
#'   myTbl$timezone <- NA
#' 
#'   dplyr::glimpse(myTbl)
#' 
#'   Sys.setenv("OPENCAGE_KEY" = "<YOUR_KEY>")
#' 
#'   table_addOpenCageInfo(myTbl) %>% 
#'     dplyr::glimpse()
#' 
#'   table_addOpenCageInfo(myTbl, replaceExisting = TRUE) %>% 
#'     dplyr::glimpse()
#' 
#'   table_addOpenCageInfo(myTbl, replaceExisting = TRUE, retainOpenCage = TRUE) %>% 
#'     dplyr::glimpse()
#'   
#' }, silent = FALSE)
#' }

table_addOpenCageInfo <- function(
  locationTbl = NULL,
  replaceExisting = FALSE,
  retainOpenCage = FALSE,
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
  
  if ( Sys.getenv("OPENCAGE_KEY") == "" )  {
    stop("Revese geocoding with OpenCage requires an API key 
Please set one with Sys.setenv(\"OPENCAGE_KEY\" = \"<YOUR_KEY>\").")
  }
  
  # ----- Reverse geocode ------------------------------------------------------
  
  openCageTbl <- tidygeocoder::reverse_geocode(
    locationTbl,
    lat = "latitude", 
    long = "longitude",  
    method = "opencage",
    address = "address",
    return_input = FALSE,
    limit = 1,
    full_results = TRUE,
    mode = "",
    unique_only = FALSE,
    return_coords = TRUE,
    min_time = NULL,
    progress_bar = FALSE,
    quiet = !verbose,
    api_url = NULL,
    timeout = 20,
    flatten = TRUE,
    batch_limit = NULL,
    verbose = verbose, # Very detailed
    no_query = FALSE,
    custom_query = list(),
    api_options = list()
  )
  
  # > dplyr::glimpse(openCageTbl, width = 75)
  # Rows: 5
  # Columns: 78
  # $ lat                                         <dbl> 43.64789, 45.18191, 4
  # $ long                                        <dbl> -116.2694, -113.8903,
  # $ address                                     <chr> "5164 North Alworth S
  # $ confidence                                  <int> 10, 9, 9, 10, 10
  # $ annotations.MGRS                            <chr> "11TNJ5890232972", "1
  # $ annotations.Maidenhead                      <chr> "DN13up75pj", "DN35be
  # $ annotations.callingcode                     <int> 1, 1, 1, 1, 1
  # $ annotations.flag                            <chr> <flag emojis>
  # $ annotations.geohash                         <chr> "9rv261z09hpr429xq8r8
  # $ annotations.qibla                           <dbl> 23.89, 25.88, 23.82, 
  # $ annotations.DMS.lat                         <chr> "43° 38' 50.70048'' N
  # $ annotations.DMS.lng                         <chr> "116° 16' 10.67196'' 
  # $ annotations.FIPS.county                     <chr> "16001", "16059", "16
  # $ annotations.FIPS.state                      <chr> "16", "16", "16", "16
  # $ annotations.Mercator.x                      <dbl> -12943076, -12678177,
  # $ annotations.Mercator.y                      <dbl> 5381538, 5619763, 537
  # $ annotations.OSM.edit_url                    <chr> "https://www.openstre
  # $ annotations.OSM.note_url                    <chr> "https://www.openstre
  # $ annotations.OSM.url                         <chr> "https://www.openstre
  # $ annotations.UN_M49.statistical_groupings    <list> "MEDC", "MEDC", "MEDC
  # $ annotations.UN_M49.regions.AMERICAS         <chr> "019", "019", "019", 
  # $ annotations.UN_M49.regions.NORTHERN_AMERICA <chr> "021", "021", "021",
  # $ annotations.UN_M49.regions.US               <chr> "840", "840", "840", 
  # $ annotations.UN_M49.regions.WORLD            <chr> "001", "001", "001", 
  # $ annotations.currency.alternate_symbols      <list> "US$", "US$", "US$", 
  # $ annotations.currency.decimal_mark           <chr> ".", ".", ".", ".", "
  # $ annotations.currency.disambiguate_symbol    <chr> "US$", "US$", "US$",
  # $ annotations.currency.html_entity            <chr> "$", "$", "$", "$", "
  # $ annotations.currency.iso_code               <chr> "USD", "USD", "USD", 
  # $ annotations.currency.iso_numeric            <chr> "840", "840", "840", 
  # $ annotations.currency.name                   <chr> "United States Dollar
  # $ annotations.currency.smallest_denomination  <int> 1, 1, 1, 1, 1
  # $ annotations.currency.subunit                <chr> "Cent", "Cent", "Cent
  # $ annotations.currency.subunit_to_unit        <int> 100, 100, 100, 100, 1
  # $ annotations.currency.symbol                 <chr> "$", "$", "$", "$", "
  # $ annotations.currency.symbol_first           <int> 1, 1, 1, 1, 1
  # $ annotations.currency.thousands_separator    <chr> ",", ",", ",", ",", "
  # $ annotations.roadinfo.drive_on               <chr> "right", "right", "ri
  # $ annotations.roadinfo.road                   <chr> "North Alworth Street
  # $ annotations.roadinfo.speed_in               <chr> "mph", "mph", "mph", 
  # $ annotations.sun.rise.apparent               <int> 1642173360, 164217312
  # $ annotations.sun.rise.astronomical           <int> 1642167300, 164216682
  # $ annotations.sun.rise.civil                  <int> 1642171500, 164217114
  # $ annotations.sun.rise.nautical               <int> 1642169340, 164216898
  # $ annotations.sun.set.apparent                <int> 1642120380, 164211948
  # $ annotations.sun.set.astronomical            <int> 1642126440, 164212578
  # $ annotations.sun.set.civil                   <int> 1642122240, 164212146
  # $ annotations.sun.set.nautical                <int> 1642124400, 164212362
  # $ annotations.timezone.name                   <chr> "America/Boise", "Ame
  # $ annotations.timezone.now_in_dst             <int> 0, 0, 0, 0, 0
  # $ annotations.timezone.offset_sec             <int> -25200, -25200, -2520
  # $ annotations.timezone.offset_string          <chr> "-0700", "-0700", "-0
  # $ annotations.timezone.short_name             <chr> "MST", "MST", "MST", 
  # $ annotations.what3words.words                <chr> "sake.pardon.topped",
  # $ bounds.northeast.lat                        <dbl> 43.64747, 45.18156, 4
  # $ bounds.northeast.lng                        <dbl> -116.2696, -113.8889,
  # $ bounds.southwest.lat                        <dbl> 43.64737, 45.17625, 4
  # $ bounds.southwest.lng                        <dbl> -116.2697, -113.8955,
  # $ `components.ISO_3166-1_alpha-2`             <chr> "US", "US", "US", "US
  # $ `components.ISO_3166-1_alpha-3`             <chr> "USA", "USA", "USA", 
  # $ components._category                        <chr> "building", "road", "
  # $ components._type                            <chr> "building", "road", "
  # $ components.continent                        <chr> "North America", "Nor
  # $ components.country                          <chr> "United States", "Uni
  # $ components.country_code                     <chr> "us", "us", "us", "us
  # $ components.county                           <chr> "Ada County", "Lemhi 
  # $ components.house_number                     <chr> "5164", NA, NA, "201"
  # $ components.postcode                         <chr> "83714", "83467", "83
  # $ components.road                             <chr> "North Alworth Street
  # $ components.state                            <chr> "Idaho", "Idaho", "Id
  # $ components.state_code                       <chr> "ID", "ID", "ID", "ID
  # $ components.town                             <chr> "Garden City", "Salmo
  # $ geometry.lat                                <dbl> 43.64742, 45.18115, 4
  # $ geometry.lng                                <dbl> -116.2696, -113.8900,
  # $ annotations.roadinfo.road_type              <chr> NA, "residential", "t
  # $ components.road_type                        <chr> NA, "residential", "t
  # $ components.village                          <chr> NA, "West Salmon", NA
  # $ components.city                             <chr> NA, NA, "Meridian", N
  
  # ----- Replace values -------------------------------------------------------
  
  # * countryCode -----
  
  if ( "components.country_code" %in% names(openCageTbl) ) {
    if ( replaceExisting ) {
      locationTbl$countryCode <- toupper(openCageTbl$components.country_code)
    } else {
      mask <- is.na(locationTbl$countryCode)
      locationTbl$countryCode[mask] <- toupper(openCageTbl$components.country_code[mask])
    }
  }
  
  # * stateCode -----
  
  if ( "components.state_code" %in% names(openCageTbl) ) {
    if ( replaceExisting ) {
      locationTbl$stateCode <- toupper(openCageTbl$components.state_code)
    } else {
      mask <- is.na(locationTbl$stateCode)
      locationTbl$stateCode[mask] <- toupper(openCageTbl$components.state_code[mask])
    }
  }
  
  # * countyName -----
  
  if ( "components.county" %in% names(openCageTbl) ) {
    if ( replaceExisting ) {
      locationTbl$countyName <- 
        stringr::str_replace(openCageTbl$components.county, " County", "")
    } else {
      mask <- is.na(locationTbl$countyName)
      locationTbl$countyName[mask] <- 
        stringr::str_replace(openCageTbl$components.county[mask], " County", "")
    }
  }
  
  # * timezone -----
  
  if ( "annotations.timezone.name" %in% names(openCageTbl) ) {
    if ( replaceExisting ) {
      locationTbl$timezone <- openCageTbl$annotations.timezone.name
    } else {
      mask <- is.na(locationTbl$timezone)
      locationTbl$timezone[mask] <- openCageTbl$annotations.timezone.name[mask]
    }
  }
  
  # * houseNumber -----
  
  if ( "components.house_number" %in% names(openCageTbl) ) {
    if ( replaceExisting ) {
      locationTbl$houseNumber <- as.character(openCageTbl$components.house_number)
    } else {
      mask <- is.na(locationTbl$houseNumber)
      locationTbl$houseNumber[mask] <- as.character(openCageTbl$components.house_number[mask])
    }
  }
  
  # * street -----
  
  if ( "components.road" %in% names(openCageTbl) ) {
    if ( replaceExisting ) {
      locationTbl$street <- as.character(openCageTbl$components.road)
    } else {
      mask <- is.na(locationTbl$street)
      locationTbl$street[mask] <- as.character(openCageTbl$components.road[mask])
    }
  }
  
  # * city -----
  
  if ( "components.town" %in% names(openCageTbl) ) {
    if ( replaceExisting ) {
      locationTbl$city <- as.character(openCageTbl$components.town)
    } else {
      mask <- is.na(locationTbl$city)
      locationTbl$city[mask] <- as.character(openCageTbl$components.town[mask])
    }
  }
  
  # NOTE:  Some OpenCage records are missing "town" but have "city" so add this
  # NOTE:  where records are still missing a value
  if ( "components.city" %in% names(openCageTbl) ) {
    mask <- is.na(locationTbl$city)
    locationTbl$city[mask] <- as.character(openCageTbl$components.city[mask])
  }
  
  # * zip -----
  
  if ( "components.postcode" %in% names(openCageTbl) ) {
    if ( replaceExisting ) {
      locationTbl$zip <- as.character(openCageTbl$components.postcode)
    } else {
      mask <- is.na(locationTbl$zip)
      locationTbl$zip[mask] <- as.character(openCageTbl$components.postcode[mask])
    }
  }
  
  # * address -----
  
  # NOTE:  'address' is not part of the core metadata but is very useful
  if ( !"address" %in% names(locationTbl) ) 
    locationTbl$address <- as.character(NA)
  
  if ( "address" %in% names(openCageTbl) ) {
    if ( replaceExisting ) {
      locationTbl$address <- as.character(openCageTbl$address)
    } else {
      mask <- is.na(locationTbl$address)
      locationTbl$address[mask] <- as.character(openCageTbl$address[mask])
    }
  }
  
  # ----- Add openCage ---------------------------------------------------------
  
  if ( retainOpenCage ) {
    
    names(openCageTbl) <- paste0("opencage_", names(openCageTbl))
    locationTbl <- dplyr::bind_cols(locationTbl, openCageTbl)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(MazamaLocationUtils)
  
  locationTbl <- 
    MazamaLocationUtils::id_monitors_500 %>%
    dplyr::slice(1:5)
  
  replaceExisting <- TRUE
  retainOpenCage <- TRUE
  verbose <- FALSE
  
}