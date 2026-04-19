
#' @title Update a column of metadata in a table
#' 
#' @description Updates records in a location table. Records are identified
#' by `locationID` and the data found in `locationData` is used to 
#' replace any existing value in the `columnName` column.
#' `locationID` and `locationData` must be of the same length.
#' Any `NA` values in `locationID` will be ignored.
#' 
#' If `columnName` is not a named column within `locationTbl`, a new
#' column will be created.
#' 
#' @param locationTbl Tibble of known locations.
#' @param columnName Name of an existing/new column in `locationTbl` whose data
#' will be updated/created.
#' @param locationID Vector of `locationID` strings.
#' @param locationData Vector of data to be inserted at records identified by 
#' `locationID`.
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
#' locationID <- 
#'   table_getLocationID(
#'     locationTbl, 
#'     wa_sub$longitude, 
#'     wa_sub$latitude, 
#'     distanceThreshold = 500
#'   )
#'   
#' locationData <- wa_sub$AQSID
#' 
#' locationTbl <- 
#'   table_updateColumn(locationTbl, "AQSID", locationID, locationData)
#' 
#' # Look at the data we attempted to merge
#' wa$AQSID[wa_indices]
#' 
#' # And two columns from the updated locationTbl
#' locationTbl_indices <- table_getRecordIndex(locationTbl, locationID)
#' locationTbl[locationTbl_indices, c("city", "AQSID")]
#' 
#' @seealso [table_addColumn()]
#' @seealso [table_removeColumn()]
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
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
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
  
  # Remove elements associated with missing locationIDs
  mask <- !is.na(locationID)
  locationID <- locationID[mask]
  locationData <- locationData[mask]
  
  # ----- Update column --------------------------------------------------------
  
  # Add it first if needed
  if ( !columnName %in% names(locationTbl) ) 
    locationTbl <- table_addColumn(locationTbl, columnName)
  
  if ( !is.null(locationData) ) {
    # Get the record indices to be updated
    recordIndex <- table_getRecordIndex(locationTbl, locationID)

    # Remove incoming locationData not associated with any record
    mask <- !is.na(recordIndex)
    recordIndex <- recordIndex[mask]
    locationData <- locationData[mask]
  
    # Updated record data
    locationTbl[[columnName]][recordIndex] <- locationData
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
