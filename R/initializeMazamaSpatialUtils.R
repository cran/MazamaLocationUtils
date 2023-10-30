#' @export
#'
#' @title Initialize MazamaSpatialUtils package
#'
#' @param spatialDataDir Directory where \pkg{MazamaSpatialUtils} datasets are found.
#'
#' @description Convenience function that wraps:
#'
#' \preformatted{
#'   MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
#'   MazamaSpatialUtils::loadSpatialData("EEZCountries.rda")
#'.  MazamaSpatialUtils::loadSpatialData("OSMTimezones.rda")
#'   MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1.rda")
#'   MazamaSpatialUtils::loadSpatialData("USCensusCounties.rda")
#' }
#' 
#' If spatial data has not yet been installed, an error is returned with an 
#' extended message detailing how to install the appropriate data.
#'
#' @examples
#' \donttest{
#' library(MazamaLocationUtils)
#' 
#' # Fail gracefully if any resources are not available
#' try({
#' 
#'   # Set up directory for spatial data
#'   spatialDataDir <- tempdir() # typically "~/Data/Spatial"
#'   MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
#' 
#'   exists("NaturalEarthAdm1")
#'   initializeMazamaSpatialUtils(spatialDataDir)
#'   exists("NaturalEarthAdm1")
#'   class(NaturalEarthAdm1)
#'   
#' }, silent = FALSE)
#' }

initializeMazamaSpatialUtils <- function(
  spatialDataDir = "~/Data/Spatial"
) {

  # ----- Validate Parameters --------------------------------------------------


  # ----- Load spatial data ----------------------------------------------------

  # Is everything already initialized?
  result <- try({
    validateMazamaSpatialUtils() # swallow warning messages
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    
    # Not initialized, so try to initialize
    result <- try({
      MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
      # Install if not found
      MazamaSpatialUtils::installSpatialData("EEZCountries")
      MazamaSpatialUtils::installSpatialData("OSMTimezones")
      MazamaSpatialUtils::installSpatialData("NaturalEarthAdm1")
      MazamaSpatialUtils::installSpatialData("USCensusCounties")
      # Now load
      MazamaSpatialUtils::loadSpatialData("EEZCountries.rda")
      MazamaSpatialUtils::loadSpatialData("OSMTimezones.rda")
      MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1.rda")
      MazamaSpatialUtils::loadSpatialData("USCensusCounties.rda")
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      # Installation failed so spit out warning messages
      validateMazamaSpatialUtils()
    }
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(NULL))
  
}
