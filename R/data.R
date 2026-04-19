#' @title Idaho monitor locations dataset
#' @format A tibble with 30 rows and 13 columns of data.
#' @description The `id_monitor_500` dataset provides a set of known
#' locations associated with Idaho state air quality monitors.
#' This dataset was generated on 2023-10-24 by running:
#' 
#' \preformatted{
#' library(AirMonitor)
#' library(MazamaLocationUtils)
#' 
#' initializeMazamaSpatialUtils()
#' setLocationDataDir("./data")
#' 
#' monitor <- monitor_loadLatest() \%>\% monitor_filter(stateCode == "ID")
#' lons <- monitor$meta$longitude
#' lats <- monitor$meta$latitude
#' 
#' table_initialize() \%>\%
#'   table_addLocation(
#'     lons, lats,
#'     distanceThreshold = 500,
#'     elevationService = "usgs",
#'     addressService = "photon"
#'   ) \%>\%
#'   table_save("id_monitors_500")
#' }
#' 
#' @seealso [or_monitors_500()]
#' @seealso [wa_monitors_500()]
"id_monitors_500"


#' @title Oregon monitor locations dataset
#' @format A tibble with 64 rows and 13 columns of data.
#' @description The `or_monitor_500` dataset provides a set of known
#' locations associated with Oregon state air quality monitors.
#' This dataset was generated on 2023-10-24 by running:
#' 
#' \preformatted{
#' library(AirMonitor)
#' library(MazamaLocationUtils)
#' 
#' initializeMazamaSpatialUtils()
#' setLocationDataDir("./data")
#'
#' monitor <- monitor_loadLatest() \%>\% monitor_filter(stateCode == "OR")
#' lons <- monitor$meta$longitude
#' lats <- monitor$meta$latitude
#' 
#' table_initialize() \%>\%
#'   table_addLocation(
#'     lons, lats, 
#'       distanceThreshold = 500,
#'       elevationService = "usgs",
#'       addressService = "photon"
#'     ) \%>\%
#'   table_save("or_monitors_500")
#' }
#' 
#' @seealso [id_monitors_500()]
#' @seealso [wa_monitors_500()]
"or_monitors_500"


#' @title Wshington monitor locations dataset
#' @format A tibble with 78 rows and 13 columns of data.
#' @description The `wa_monitor_500` dataset provides a set of known
#' locations associated with Washington state air quality monitors.
#' This dataset was generated on 2023-10-24 by running:
#' 
#' \preformatted{
#' library(AirMonitor)
#' library(MazamaLocationUtils)
#' 
#' initializeMazamaSpatialUtils()
#' setLocationDataDir("./data")
#' 
#' monitor <- monitor_loadLatest() \%>\% monitor_filter(stateCode == "WA")
#' lons <- monitor$meta$longitude
#' lats <- monitor$meta$latitude
#' 
#' table_initialize() \%>\%
#'   table_addLocation(
#'     lons, lats,
#'     distanceThreshold = 500,
#'     elevationService = "usgs",
#'     addressService = "photon"
#'   ) \%>\%
#'   table_save("wa_monitors_500")
#' }
#' 
#' @seealso [id_monitors_500()]
#' @seealso [or_monitors_500()]
"wa_monitors_500"


#' @title Washington monitor metadata dataset
#' @format A tibble with 92 rows and 29 columns of data.
#' @description The `wa_pwfsl_meta` dataset provides a set of Washington
#' state air quality monitor metadata used by the USFS AirFire group.
#' This dataset was generated on 2023-10-24 by running:
#' 
#' \preformatted{
#' library(AirMonitor)
#' 
#' wa_airfire_meta <-
#'   airnow_loadLatest() \%>\%
#'   monitor_filter(stateCode == "WA") \%>\%
#'   monitor_getMeta() \%>\%
#'   # On 2023-10-24, this metdata still uses zip instead of postalCode
#'   dplyr::rename(postalCode = zip) \%>\%
#'   # Remove internal fields
#'   dplyr::select(-dplyr::starts_with("airnow_"))
#' 
#' save(wa_airfire_meta, file = "data/wa_airfire_meta.rda")
#' }
"wa_airfire_meta"

