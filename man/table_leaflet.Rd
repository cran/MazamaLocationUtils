% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_leaflet.R
\name{table_leaflet}
\alias{table_leaflet}
\title{Leaflet interactive map for known locations}
\usage{
table_leaflet(
  locationTbl = NULL,
  maptype = c("terrain", "roadmap", "satellite", "toner"),
  extraVars = NULL,
  jitter = 5e-04,
  ...
)
}
\arguments{
\item{locationTbl}{Tibble of known locations.}

\item{maptype}{Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.}

\item{extraVars}{Character vector of addition \code{locationTbl} column names
to be shown in leaflet popups.}

\item{jitter}{Amount to use to slightly adjust locations so that multiple
monitors at the same location can be seen. Use zero or \code{NA} to see
precise locations.}

\item{...}{Additional arguments passed to \code{leaflet::addCircleMarker()}.}
}
\value{
A leaflet "plot" object which, if not assigned, is rendered in
Rstudio's 'Viewer' tab.
}
\description{
This function creates interactive maps that will be displayed in
RStudio's 'Viewer' tab. The default setting of \code{jitter} will move locations
randomly within an ~50 meter radius so that overlapping locations can be
identified. Set \code{jitter = 0} to see precise locations.
}
\details{
The \code{maptype} argument is mapped onto leaflet "ProviderTile"
names. Current mappings include:
\itemize{
\item{"roadmap" => "OpenStreetMap"}
\item{"satellite" => "Esri.WorldImagery"}
\item{"terrain" => "Esri.WorldTopoMap"}
\item{"toner" => "Stamen.Toner"}
}

If a character string not listed above is provided, it will be used as the
underlying map tile if available. See
\url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
"provider tiles" to use as the background map.
}
\examples{
\dontrun{
library(MazamaLocationUtils)

# A table with all core metadata
table_leaflet(wa_monitors_500)
  
# A table missing some core metadata
table_leaflet(
  wa_airfire_meta,
  extraVars = c("stateCode", "countyName", "msaName")
)

# Customizing the map
table_leaflet(
  wa_airfire_meta,
  extraVars = c("stateCode", "countyName", "msaName"),
  radius = 6,
  color = "black",
  weight = 2,
  fillColor = "red",
  fillOpacity = 0.3
)
}
}
