% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_createID.R
\name{location_createID}
\alias{location_createID}
\title{Create one or more unique locationIDs}
\usage{
location_createID(longitude = NULL, latitude = NULL)
}
\arguments{
\item{longitude}{Vector of longitudes in decimal degrees E, Default: NULL}

\item{latitude}{Vector of latitudes in decimal degrees N, Default: NULL}
}
\value{
Vector of character locationIDs.
}
\description{
A unique locationID is created for each incoming
\code{longitude} and \code{latitude}. The following code is used to generate
each locationID. See the references for details.

\preformatted{
# Retain accuracy up to ~.1m
locationString <- paste0(
  sprintf("\%.7f", longitude),
  "_",
  sprintf("\%.7f", latitude)
)
  
# Avoid collisions until billions of records
locationID <- digest::digest(locationString, algo = "xxhash64")  
}
}
\examples{
library(MazamaLocationUtils)

# Wenatchee
lon <- -120.325278
lat <- 47.423333
locationID <- location_createID(lon, lat)
}
\references{
\url{https://en.wikipedia.org/wiki/Decimal_degrees}

\url{https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/}
}
