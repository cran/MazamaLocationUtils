
#' @title Create an empty known location table
#' 
#' @description Creates an empty known location tibble with the following 
#' columns of core metadata:
#' 
#' \itemize{
#' \item{locationID}
#' \item{locationName}
#' \item{longitude}
#' \item{latitude}
#' \item{elevation}
#' \item{countryCode}
#' \item{stateCode}
#' \item{county}
#' \item{timezone}
#' \item{houseNumber}
#' \item{street}
#' \item{city}
#' \item{zip}
#' }
#' 
#' @return Empty known location tibble with the specified metadata columns.
#' 
#' @examples 
#' library(MazamaLocationUtils)
#' 
#' # Create an empty Tbl
#' emptyTbl <- table_initialize()
#' print(emptyTbl)
#' @rdname table_initialize
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter
#' @importFrom rlang .data
table_initialize <- function() {
  
  # ----- Validate parameters --------------------------------------------------
  

  # ----- Create empty tibble --------------------------------------------------
  
  # Build up a tibble with a single record full of NAs
  locationTbl <- dplyr::tibble(
    "locationID" = as.character(NA),
    "locationName" = as.character(NA),
    "longitude" = as.numeric(NA),
    "latitude" = as.numeric(NA),
    "elevation" = as.numeric(NA),
    "countryCode" = as.character(NA),
    "stateCode" = as.character(NA),
    "county" = as.character(NA),
    "timezone" = as.character(NA),
    "houseNumber" = as.character(NA),
    "street" = as.character(NA),
    "city" = as.character(NA),
    "zip" = as.character(NA)
  )
  
  # Now search for an ID we won't find to end up with an empty tibble with 
  # the correct column names.
  locationTbl <-
    locationTbl %>%
    dplyr::filter(.data$locationID == "Rumplestiltskin")

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
