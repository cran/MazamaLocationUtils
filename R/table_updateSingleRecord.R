
#' @title Update a single known location record in a table
#' @description Information in the \code{locationList} is used to replace
#' existing information found in \code{locationTbl}. This function can be used
#' for small tweaks to an existing \code{locationTbl}. Wholesale replacement of
#' records should be performed with \code{table_removeRecord()} followed by
#' \code{table_addLocation()}. 
#' @param locationTbl Tibble of known locations.
#' @param locationList List containing `locationID` and one or more named
#' columns whose values are to be replaced.
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @examples
#' library(MazamaLocationUtils)
#' 
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Wenatchee
#' wenatcheeRecord <- 
#'   locationTbl %>% 
#'   dplyr::filter(city == "Wenatchee")
#' 
#' str(wenatcheeRecord)
#' 
#' wenatcheeID <- wenatcheeRecord$locationID
#' 
#' locationTbl <- table_updateSingleRecord(
#'   locationTbl,
#'   locationList = list(
#'     locationID = wenatcheeID,
#'     locationName = "Wenatchee-Fifth St"
#'   )
#' )
#' 
#' # Look at the new record
#' locationTbl %>% 
#'   dplyr::filter(city == "Wenatchee") %>%
#'   str()
#' 
#' @seealso \link{table_addLocation}
#' @seealso \link{table_addSingleLocation}
#' @seealso \link{table_removeRecord}
#' @rdname table_updateSingleRecord
#' @export 
#' @importFrom rlang .data
table_updateSingleRecord <- function(
  locationTbl = NULL,
  locationList = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
  MazamaCoreUtils::stopIfNull(locationList)

  invalidNames <- setdiff(names(locationList), names(locationTbl))
  if ( length(invalidNames) > 0 ) {
    invalidString <- paste0(invalidNames, collapse = ", ")
    stop(sprintf(
      "Invalid names found in locationList: %s", invalidString
    ))
  }
  
  if ( !locationList$locationID %in% locationTbl$locationID ) 
    stop(sprintf(
      "locationID %s is not found in locationTbl", locationList$locationID
    ))
  
  # ----- Update locationTbl ---------------------------------------------------
  
  row <- which(locationTbl$locationID == locationList$locationID)
  
  if ( length(row) > 0 ) {
    
    for ( columnName in names(locationList) ) {
      locationTbl[row, columnName] <- locationList[[columnName]]
    }
    
  } else {
    
    if ( verbose ) {
      warning(sprintf(
        "No location found for %s", locationList$locationID
      ))
    }
    
  }

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
