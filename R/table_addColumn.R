#'
#' @title Add a new column of metadata to a table
#' 
#' @description A new metadata column is added to the \code{locationTbl}. For
#' matching \code{locationID} records, the associated \code{locationData} is
#' inserted. Otherwise, the new column will be initialized with \code{NA}.
#' 
#' @param locationTbl Tibble of known locations.
#' @param columnName Name to use for the new column.
#' @param locationID Vector of \code{locationID} strings.
#' @param locationData Vector of data to used at matching records.
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return Updated tibble of known locations.
#' 
#' @examples
#' library(MazamaLocationUtils)
#' 
#' # Starting table
#' locationTbl <- get(data("wa_monitors_500")) 
#' names(locationTbl)
#' 
#' # Add an empty column
#' locationTbl <-
#'   locationTbl %>%
#'   table_addColumn("AQSID")
#'   
#' names(locationTbl)
#' 
#' @seealso \link{table_removeColumn}
#' @seealso \link{table_updateColumn}
#' @rdname table_addColumn
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom methods as
#' 
table_addColumn <- function(
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
    dataClass <- class(locationData)
  } else {
    dataClass <- "character"
  }
  
  # ----- Add new column -------------------------------------------------------
  
  locationTbl[[columnName]] <- methods::as(rep(NA, nrow(locationTbl)), dataClass)
  
  # Update if any data are passed in
  if ( !is.null(locationID) ) {
    locationTbl <- 
      table_updateColumn(locationTbl, columnName, locationID, locationData)
  }

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
