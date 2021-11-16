#' @rdname mazama_initialize
#' 
#' @export 
#' @importFrom MazamaSpatialUtils setSpatialDataDir loadSpatialData
#' 
#' @title Initialize with MazamaScience standard directories
#' 
#' @param spatialDataDir Directory where spatial datasets are found, 
#' Default: "~/Data/Spatial"
#' 
#' @return No return value.
#' 
#' @description Convenience function to initialize spatial data. Wraps the 
#' following setup lines:
#' 
#' \preformatted{
#' MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
#' 
#' MazamaSpatialUtils::loadSpatialData("EEZCountries")
#' MazamaSpatialUtils::loadSpatialData("OSMTimezones")
#' MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")
#' MazamaSpatialUtils::loadSpatialData("USCensusCounties")
#' }
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
#'   mazama_initialize(spatialDataDir)
#'   exists("NaturalEarthAdm1")
#'   class(NaturalEarthAdm1)
#'   
#' }, silent = FALSE)
#' }
mazama_initialize <- function(
  spatialDataDir = "~/Data/Spatial"
) {
  
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
      MazamaSpatialUtils::loadSpatialData("EEZCountries")
      MazamaSpatialUtils::loadSpatialData("OSMTimezones")
      MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")
      MazamaSpatialUtils::loadSpatialData("USCensusCounties")
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      # Installation failed so spit out warning messages
      validateMazamaSpatialUtils()
    }
   
  }
  
}