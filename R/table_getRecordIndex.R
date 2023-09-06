
#' @title Return indexes of known location records
#' @description Returns a vector of \code{locationTbl} row indexes for the 
#' locations associated with each \code{locationID}.
#' @param locationTbl Tibble of known locations.
#' @param locationID Vector of \code{locationID} strings.
#' @param verbose Logical controlling the generation of progress messages.
#' @return Vector of \code{locationTbl} row indexes.
#' @examples
#' library(MazamaLocationUtils)
#' 
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' 
#' # Get the locationID first
#' locationID <- table_getLocationID(locationTbl, lon, lat, distanceThreshold = 5000)
#' 
#' # Now find the row associated with this ID
#' recordIndex <- table_getRecordIndex(locationTbl, locationID)
#' 
#' str(locationTbl[recordIndex,])
#' 
#' @rdname table_getRecordIndex
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr bind_rows
table_getRecordIndex <- function(
  locationTbl = NULL,
  locationID = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
  MazamaCoreUtils::stopIfNull(locationID)
  
  # ----- Find recordIndexes ---------------------------------------------------
  
  recordIndex <- rep(as.numeric(NA), length(locationID))
    
  for ( index in seq_along(locationID) ) {
    
    if ( !is.na(locationID[index]) ) {
      if ( any(locationTbl$locationID == locationID[index], na.rm = TRUE) ) {
        recordIndex[index] <- which(locationTbl$locationID == locationID[index])
      }
    }
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(recordIndex)
  
}
