#' @title Idaho monitor locations dataset
#' @format A tibble with 34 rows and 13 columns of data.
#' @description The \code{id_monitor_500} dataset provides a set of known
#' locations associated with Idaho state air quality monitors.
#' This dataset was generatedon 2019-10-21 by running:
#' 
#' \preformatted{
#' library(PWFSLSmoke)
#' library(MazamaLocationUtils)
#' 
#' mazama_initialize()
#' setLocationDataDir("./data")
#'
#' monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "ID")
#' lons <- monitor$meta$longitude
#' lats <- monitor$meta$latitude
#' 
#' table_initialize() %>%
#'   table_addLocation(lons, lats, distanceThreshold = 500) %>%
#'   table_save("id_monitors_500")
#' }
#' 
#' @seealso \link{or_monitors_500}
#' @seealso \link{wa_monitors_500}
"id_monitors_500"


#' @title Oregon monitor locations dataset
#' @format A tibble with 40 rows and 13 columns of data.
#' @description The \code{or_monitor_500} dataset provides a set of known
#' locations associated with Oregon state air quality monitors.
#' This dataset was generatedon 2019-10-21 by running:
#' 
#' \preformatted{
#' library(PWFSLSmoke)
#' library(MazamaLocationUtils)
#' 
#' mazama_initialize()
#' setLocationDataDir("./data")
#'
#' monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "OR")
#' lons <- monitor$meta$longitude
#' lats <- monitor$meta$latitude
#' 
#' table_initialize() %>%
#'   table_addLocation(lons, lats, distanceThreshold = 500) %>%
#'   table_save("or_monitors_500")
#' }
#' 
#' @seealso \link{id_monitors_500}
#' @seealso \link{wa_monitors_500}
"or_monitors_500"


#' @title Wshington monitor locations dataset
#' @format A tibble with 69 rows and 13 columns of data.
#' @description The \code{wa_monitor_500} dataset provides a set of known
#' locations associated with Washington state air quality monitors.
#' This dataset was generatedon 2019-10-21 by running:
#' 
#' \preformatted{
#' library(PWFSLSmoke)
#' library(MazamaLocationUtils)
#' 
#' mazama_initialize()
#' setLocationDataDir("./data")
#'
#' monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "WA")
#' lons <- monitor$meta$longitude
#' lats <- monitor$meta$latitude
#' 
#' table_initialize() %>%
#'   table_addLocation(lons, lats, distanceThreshold = 500) %>%
#'   table_save("wa_monitors_500")
#' }
#' 
#' @seealso \link{id_monitors_500}
#' @seealso \link{or_monitors_500}
"wa_monitors_500"


#' @title Washington monitor metadata dataset
#' @format A tibble with 69 rows and 19 columns of data.
#' @description The \code{wa_pwfsl_meta} dataset provides a set of Washington
#' state air quality monitor metadata used by the USFS AirFire group.
#' This dataset was generatedon 2019-10-21 by running:
#' 
#' \preformatted{
#' library(PWFSLSmoke)
#' 
#' wa_airfire_meta <- 
#'   monitor_loadLatest() %>% 
#'   monitor_subset(stateCodes = "WA") %>%
#'   monitor_extractMeta()
#'  
#' save(wa_airfire_meta, file = "data/wa_airfire_meta.rda")
#' }
"wa_airfire_meta"

