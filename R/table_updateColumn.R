
#' @title Update a column of metadata in a table
#' @description For matching \code{locationID} records the assaociated 
#' \code{locatioData} is used to replace any existing value in \code{columnName}.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param columnName Name to use for the new column, Default: NULL
#' @param locationID Vector of \code{locationID} strings, Default: NULL
#' @param locationData Vector of data to used at matching records, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @examples
#' library(MazamaLocationUtils)
#' 
#' locationTbl <- get(data("wa_monitors_500"))
#' wa <- get(data("wa_airfire_meta"))
#' 
#' # We will merge some metadata from wa into locationTbl
#' 
#' # Record indices for wa
#' wa_indices <- seq(5,65,5)
#' wa_sub <- wa[wa_indices,]
#' 
#' locationID <- table_getLocationID(locationTbl, wa_sub$longitude, wa_sub$latitude, radius = 500)
#' locationData <- wa_sub$siteName
#' 
#' locationTbl <- table_updateColumn(locationTbl, "siteName", locationID, locationData)
#' 
#' # Look at the data we attempted to merge
#' wa$siteName[wa_indices]
#' 
#' # And two columns from the updated locationTbl
#' locationTbl_indices <- table_getRecordIndex(locationTbl, locationID)
#' locationTbl[locationTbl_indices, c("city","siteName")]
#' 
#' @seealso \link{table_addColumn}
#' @seealso \link{table_removeColumn}
#' @rdname table_updateColumn
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr bind_rows
table_updateColumn <- function(
  locationTbl = NULL,
  columnName = NULL,
  locationID = NULL,
  locationData = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(columnName)
  
  if ( !is.null(locationID) )
    MazamaCoreUtils::stopIfNull(locationData)
  
  if ( !is.null(locationData) )
    MazamaCoreUtils::stopIfNull(locationID)
  
  if ( !is.null(locationID) && !is.null(locationData) ) {
    if ( length(locationID) != length(locationData) ) {
      stop(sprintf(
        "locationID and locationData must have the same length"
      ))
    }
  }
  
  # ----- Update column --------------------------------------------------------
  
  # Add it first if needed
  if ( !columnName %in% names(locationTbl) ) 
    locationTbl <- table_addColumn(locationTbl, columnName)
  
  if ( !is.null(locationData) ) {
    # Get the indices to be updated
    recordIndex <- table_getRecordIndex(locationTbl, locationID)
    locationTbl[[columnName]][recordIndex] <- locationData
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
