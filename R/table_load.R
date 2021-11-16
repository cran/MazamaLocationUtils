
#' @title Load a known location table
#' @description Load a tibble of known locations from the preferred directory.
#' @param collectionName Character identifier for this table.
#' @return Tibble of known locations.
#' @examples
#' library(MazamaLocationUtils)
#' 
#' # Set the directory for saving location tables
#' setLocationDataDir(tempdir())
#' 
#' # Load an example table and check the dimensions
#' locationTbl <- get(data("wa_monitors_500"))
#' dim(locationTbl)
#' 
#' # Save it as "table_load_example"
#' table_save(locationTbl, "table_load_example")
#' 
#' # Load it and check the dimensions
#' my_table <- table_load("table_load_example")
#' dim(my_table)
#' 
#' # Check the locationDataDir
#' list.files(getLocationDataDir(), pattern = "table_load_example")
#' @seealso 
#'  \code{\link{setLocationDataDir}}
#' @rdname table_load
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
table_load <- function(
  collectionName = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(collectionName)
  
  dataDir <- getLocationDataDir()
  
  # ----- Load data ------------------------------------------------------------
  
  result <- try({
    
    fileName <- paste0(collectionName, ".rda")
    filePath <- file.path(dataDir, fileName)
    locationTbl <- get(load(filePath))
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    stop(sprintf("Could not load %s", filePath))
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
