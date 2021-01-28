#'
#' @docType package
#' @name MazamaLocationUtils
#' @title Manage Spatial Metadata for Known Locations
#' @description A suite of utility functions for discovering and managing 
#' metadata associated with sets of spatially unique "known locations".
#' 
#' This package is intended to be used in support of data management activities
#' associated with fixed locations in space. The motivating fields include both
#' air and water quality monitoring where fixed sensors report at regular time 
#' intervals.
#'
#' @details
#' When working with environmental monitoring time series, one of the first things
#' you have to do is create unique identifiers for each individual time series. In 
#' an ideal world, each environmental time series would have both a 
#' \code{locationID} and a \code{sensorID} that uniquely identify the spatial location and 
#' specific instrument making measurements. A unique \code{timeseriesID} could
#' be produced as \code{locationID_sensorID}. Metadata associated with each
#' time series would contain basic information needed for downstream analysis
#' including at least:
#' \preformatted{
#'   timeseriesID, locationID, sensorID, longitude, latitude, ...
#' }
#' 
#' \itemize{
#' \item{Multiple sensors placed at a location could be be grouped by \code{locationID}.}
#' \item{An extended timeservers for a mobile sensor would group by \code{sensorID}.}
#' \item{Maps would be created using \code{longitude, latitude}.}
#' \item{Time series would be accessed from a secondary \code{data} table with \code{timeseriesID}.}
#' }
#' 
#' Unfortunately, we are rarely supplied with a truly unique and truly spatial 
#' \code{locationID}. Instead we often use \code{sensorID} or an associated non-spatial
#' identifier as a standin for \code{locationID}.
#' 
#' Complications we have seen include:
#'   
#' \itemize{
#' \item{GPS-reported longitude and latitude can have _jitter_ in the fourth or fifth 
#' decimal place making it challenging to use them to create a unique \code{locationID}.}
#' \item{Sensors are sometimes _repositioned_ in what the scientist considers the "same 
#' location".}
#' \item{Data for a single sensor goes through different processing pipelines using
#' different identifiers and is later brought together as two separate timeseries.}
#' \item{The radius of what constitutes a "single location" depends on the 
#' instrumentation and scientific question being asked.}
#' \item{Deriving location-based metadata from spatial datasets is computationally 
#' intensive unless saved and identified with a unique \code{locationID}.}
#' \item{Automated searches for spatial metadata occasionally produce incorrect results
#' because of the non-infinite resolution of spatial datasets.}
#' }
#' 
#' This package attempts to address all of these issues by maintaining a table
#' of known locations for which CPU intensive spatial data calculations have
#' already been performed. While requests to add new locations to the table may
#' take some time, searches for spatial metadata associated with existing
#' locations are simple lookups.
#' 
#' Working in this manner will solve the problems initially mentioned but also 
#' provides further useful functionality.
#' 
#' \itemize{
#' \item{Administrators can correct entries in the \code{collectionName} table.  
#' (_e.g._ locations in river bends that even high resolution spatial datasets mis-assign)}
#' \item{Additional, non-automatable metadata can be added to \code{collectionName}. 
#' (_e.g._ commonly used location names within a community of practice)}
#' \item{Different field campaigns can have separate \code{collectionName} tables.}
#' \item{\code{.csv} or \code{.rda} versions of well populated tables can be downloaded from a
#' URL and used locally, giving scientists working with known locations instant
#' access to spatial data that otherwise requires special skills, large datasets 
#' and lots of compute cycles.}
#' }

NULL

# ----- Internal Data -------------------------------------------------

#' coreMetadataNames
#'
#' @export
#' @docType data
#' @name coreMetadataNames
#' @title Names of standard spatial metadata columns
#' @format A vector with 3 elements
#' @description Character string identifiers of the different types of spatial 
#' metadata this package can generate.

coreMetadataNames <- c(
  "locationID",               # from MazamaLocationUtils::location_createID()
  "locationName",             # from MazamaLocationUtils::location_initialize()
  "longitude",                # user supplied
  "latitude",                 # user supplied
  "elevation",                # from https://nationalmap.gov/epqs/
  "countryCode",              # from MazamaSpatialUtils::getCountryCode()
  "stateCode",                # from MazamaSpatialUtils::getStateCode()
  "county",                   # from MazamaSpatialUtils::getUSCounty()
  "timezone",                 # from MazamaSpatialUtils::getTimezone()
  "houseNumber",              # from https://photon.komoot.io/
  "street",                   # from https://photon.komoot.io/
  "city",                     # from https://photon.komoot.io/
  "zip"                       # from https://photon.komoot.io/
)

# ----- Internal Package State -------------------------------------------------

MazamaLocationUtilsEnv <- new.env(parent = emptyenv())
MazamaLocationUtilsEnv$dataDir <- NULL
MazamaLocationUtilsEnv$apiKeys <- list(
  "google" = NULL,
  "bing" = NULL,
  "esri" = NULL,
  "texasam" = NULL,
  "custom1" = NULL,
  "custom2" = NULL
)

# ----- API Keys ---------------------------------------------------------------

#' @docType data
#' @keywords environment
#' @name apiKeys
#' @title API keys for reverse geocoding services.
#' @format List of character strings.
#' @description This package maintains an internal set of API keys which 
#' users can set using \code{setAPIKey()}. These keys will be remembered for
#' the duration of an R session. The following service providers are supported:
#' 
#' \itemize{
#' \item{\code{"google"}}
#' \item{\code{"bing"}}
#' \item{\code{"esri"}}
#' \item{\code{"texasam"}}
#' \item{\code{"custom1"}}
#' \item{\code{"custom2"}}
#' }
#' 
#' @seealso \link{getAPIKey}
#' @seealso \link{setAPIKey}
NULL

#' @keywords environment
#' @export
#' @title Get API key
#' @param provider Web service provider.
#' @description Returns the API key associated with a web service.
#' If \code{provider == NULL} a list is returned containing all recognized
#' API keys.
#' @return API key string or a list of provider:key pairs.
#' @seealso \link{apiKeys}
#' @seealso \link{setAPIKey}

getAPIKey <- function(provider = NULL) {
  if ( is.null(provider) ) {
    return(MazamaLocationUtilsEnv$apiKeys)
  } else {
    if ( !(provider %in% names(MazamaLocationUtilsEnv$apiKeys)) ) {
      stop(sprintf(
        "Provider \"%s\" is not recognized.", provider
      ),
      call.=FALSE)
    } else {
      return(MazamaLocationUtilsEnv$apiKeys[[provider]])
    }
  }
}

#' @keywords environment
#' @export
#' @title Set APIKey
#' @param provider Web service provider.
#' @param key API key.
#' @description Sets the API key associated with a web service.
#' @return Silently returns previous value of the API key.
#' @seealso \link{LocationDataDir}
#' @seealso \link{getLocationDataDir}

setAPIKey <- function(provider = NULL, key = NULL) {
  if ( !(provider %in% names(MazamaLocationUtilsEnv$apiKeys)) ) {
    stop(sprintf(
      "Provider \"%s\" is not recognized.", provider
    ),
    call.=FALSE)
  } else {
    old <- MazamaLocationUtilsEnv$apiKeys[[provider]]
    MazamaLocationUtilsEnv$apiKeys[[provider]] <- key
    return(invisible(old))
  }
}

# ----- Data Directory Configuration -------------------------------------------

#' @docType data
#' @keywords environment
#' @name LocationDataDir
#' @title Directory for location data
#' @format Absolute path string.
#' @description This package maintains an internal directory path which 
#' users can set using \code{setLocationDataDir()}. All package functions use 
#' this directory whenever known location tables are accessed.
#' 
#' The default setting when the package is loaded is \code{getwd()}.
#' @seealso \link{getLocationDataDir}
#' @seealso \link{setLocationDataDir}
NULL

#' @keywords environment
#' @export
#' @title Get location data directory
#' @description Returns the directory path where known location data tables are located.
#' @return Absolute path string.
#' @seealso \link{LocationDataDir}
#' @seealso \link{setLocationDataDir}

getLocationDataDir <- function() {
  if (is.null(MazamaLocationUtilsEnv$dataDir)) {
    stop(paste0('No data directory found. Please set a data directory with ',
                'setLocationDataDir("YOUR_DATA_DIR")'),
         call.=FALSE)
  } else {
    return(MazamaLocationUtilsEnv$dataDir)
  }
}

#' @keywords environment
#' @export
#' @title Set location data directory
#' @param dataDir Directory where location tables are stored.
#' @description Sets the data directory where known location data tables are located.
#' If the directory does not exist, it will be created.
#' @return Silently returns previous value of the data directory.
#' @seealso \link{LocationDataDir}
#' @seealso \link{getLocationDataDir}

setLocationDataDir <- function(dataDir) {
  old <- MazamaLocationUtilsEnv$dataDir
  dataDir <- path.expand(dataDir)
  tryCatch({
    if (!file.exists(dataDir)) dir.create(dataDir)
    MazamaLocationUtilsEnv$dataDir <- dataDir
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error in setLocationDataDir(",dataDir,")."))
  })
  return(invisible(old))
}
