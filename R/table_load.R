
#' @title Load a known location table
#' @description Load a tibble of known locations from the preferred directory.
#' 
#' The known location table must be named either \code{<collectionName>.rda}
#' or \code{<collectionName>.csv}. If both are found, only 
#' \code{<collectionName>.rda} will be loaded to ensure that columns will have
#' the proper type assigned.
#' 
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
    
    # First try the .rda version
    fileName <- paste0(collectionName, ".rda")
    filePath <- file.path(dataDir, fileName)
    if ( file.exists(filePath) ) {
      locationTbl <- get(load(filePath))
    } else {
      # Then try the .csv version
      fileName <- paste0(collectionName, ".csv")
      filePath <- file.path(dataDir, fileName)
      if ( file.exists(filePath) ) {
        locationTbl <- readr::read_csv(
          filePath,
          progress = FALSE,
          show_col_types = FALSE
        )
      } else {
        stop(sprintf("No '%s' collection table found in ", collectionName, dataDir))
      }
    }
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    stop(sprintf("Could not load %s", filePath))
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
