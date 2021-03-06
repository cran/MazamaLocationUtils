% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{wa_monitors_500}
\alias{wa_monitors_500}
\title{Wshington monitor locations dataset}
\format{
A tibble with 69 rows and 13 columns of data.
}
\usage{
wa_monitors_500
}
\description{
The \code{wa_monitor_500} dataset provides a set of known
locations associated with Washington state air quality monitors.
This dataset was generatedon 2019-10-21 by running:

\preformatted{
library(PWFSLSmoke)
library(MazamaLocationUtils)

mazama_initialize()
setLocationDataDir("./data")

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "WA")
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

table_initialize() %>%
  table_addLocation(lons, lats, radius = 500) %>%
  table_save("wa_monitors_500")
}
}
\seealso{
\link{id_monitors_500}

\link{or_monitors_500}
}
\keyword{datasets}
