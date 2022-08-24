#' @rdname location_getOpenCageInfo
#' @export 
#'
#' @title Get location information from OpenCage
#' 
#' @description The OpenCage reverse geocoding service is used to obtain all 
#' available information for a specific location.
#' 
#' The data from OpenCage should be considered to be the gold standard for address
#' information could and should be used to override information we get elsewhere.
#' 
#' @note The OpenCage service requires an API key which can be obtained from 
#' their web site. This API key must be set as an environment variable with:
#' 
#' \preformatted{
#' Sys.setenv("OPENCAGE_KEY" = "<your api key>")
#' }
#' 
#' The OpenCage "free trial" level allows for 1 request/sec and a maximum of 
#' 2500 requests per day.
#' 
#' @param longitude Single longitude in decimal degrees E.
#' @param latitude Single latitude in decimal degrees N.
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return Single-row tibble with OpenCage information.
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
#'   Sys.setenv("OPENCAGE_KEY" = "<YOUR_KEY>")
#' 
#'   # Wenatchee
#'   lon <- -120.325278
#'   lat <- 47.423333
#' 
#'   openCageTbl <- location_getOpenCageInfo(lon, lat)
#'   dplyr::glimpse(openCageTbl)
#'   
#' }, silent = FALSE)
#' }
#' 
location_getOpenCageInfo <- function(
  longitude = NULL,
  latitude = NULL,
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  
  
  if ( Sys.getenv("OPENCAGE_KEY") == "" )  {
    stop("Revese geocoding with OpenCage requires an API key 
Please set one with Sys.setenv(\"OPENCAGE_KEY\" = \"<YOUR_KEY>\").")
  }
  
  # ----- Reverse geocode ------------------------------------------------------
  
  openCageTbl <- tidygeocoder::reverse_geo(
    latitude, 
    longitude,  
    method = "opencage",
    address = "address",
    limit = 1,
    full_results = TRUE,
    mode = "",
    unique_only = FALSE,
    return_coords = TRUE,
    min_time = NULL,
    progress_bar = verbose,
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
  

  # ----- Fixes ----------------------------------------------------------------
  
  # NOTE:  Fix any column types (typically <int> or <dbl> needs to become <character>)
  
  if ( "components.road_reference" %in% names(openCageTbl) ) {
    openCageTbl$components.road_reference <-
      as.character(openCageTbl$components.road_reference)
  }
  
  if ( "components.building" %in% names(openCageTbl) ) {
    cat(sprintf("location with 'building'= %f, %f\n", longitude, latitude))
    openCageTbl$components.building <-
      as.character(openCageTbl$components.building)
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(openCageTbl)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(MazamaLocationUtils)
  
  locationTbl <- 
    MazamaLocationUtils::id_monitors_500
  
  i <- 22
  longitude <- locationTbl$longitude[i]
  latitude <- locationTbl$latitude[i]
  verbose <- TRUE
  
  
  
}