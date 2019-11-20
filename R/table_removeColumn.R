
#' @title Remove a column of metadata in a table
#' @description Remove the column matching \code{columnName}. This function 
#' can be used in pipelines.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param columnName Name of the colun to be removed, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @examples
#' # Starting table
#' locationTbl <- get(data("wa_monitors_500")) 
#' names(locationTbl)
#' 
#' # Add a new column
#' locationTbl <-
#'   locationTbl %>%
#'   table_addColumn("siteName")
#'   
#' names(locationTbl)
#'
#' # Now remove it
#' locationTbl <-
#'   locationTbl %>%
#'   table_removeColumn("siteName")
#' 
#' names(locationTbl)
#' 
#' \dontrun{
#' # Cannot remove "core" metadata
#' locationTbl <-
#'   locationTbl %>%
#'   table_removeColumn("zip")
#'}
#' @seealso \link{table_addColumn}
#' @seealso \link{table_removeColumn}
#' @rdname table_removeColumn
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
table_removeColumn <- function(
  locationTbl = NULL,
  columnName = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(columnName)
  
  if ( !columnName %in% names(locationTbl) ) 
    stop(sprintf(
      "columnName %s is not found in locationTbl", columnName
    ))
  
  if ( columnName %in% MazamaLocationUtils::coreMetadataNames )
    stop(sprintf(
      "columnName %s is part of the core metdata and cannot be removed", columnName
    ))
  
  
  # ----- Remove column --------------------------------------------------------
  
  locationTbl[[columnName]] <- NULL
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
