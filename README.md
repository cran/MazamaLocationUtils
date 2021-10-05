[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/MazamaLocationUtils)](https://cran.r-project.org/package=MazamaLocationUtils)
[![Downloads](http://cranlogs.r-pkg.org/badges/MazamaLocationUtils)](https://cran.r-project.org/package=MazamaLocationUtils)
[![Build Status](https://travis-ci.org/MazamaScience/MazamaLocationUtils.svg?branch=master)](https://travis-ci.org/MazamaScience/MazamaLocationUtils)


# MazamaLocationUtils

```
A suite of utility functions for discovering and managaing metadata associated
with sets of spatially unique "known locations".
```

## Background

This package is intended to be used in support of data management activities
associated with fixed locations in space. The motivating fields include both
air and water quality monitoring where fixed sensors report at regular time 
intervals.

When working with environmental monitoring time series, one of the first things
you have to do is create unique identifiers for each individual time series. In 
an ideal world, each environmental time series would have both a 
`locationID` and a `sensorID` that uniquely identify the spatial location and 
specific instrument making measurements. A unique `timeseriesID` could
be produced as `locationID_sensorID`. Metadata associated with each
timeseries would contain basic information needed for downstream analysis
including at least:

`timeseriesID, locationID, sensorID, longitude, latitude, ...`

* Multiple sensors placed at a single location could be associated based on their shared `locationID`.
* An extended time series for a single, potentially redeployed sensor would be associated with a single `sensorID`.
* Maps could be created using `longitude, latitude` fields.
* Time series data for a sensor would be accessed from a secondary `data` table with `timeseriesID` column names.

Unfortunately, we are rarely supplied with a truly unique and truly spatial 
`locationID`. Instead we often use `sensorID` or an associated non-spatial
identifier as a stand-in for `locationID`.

Complications we have seen include:

* GPS-reported longitude and latitude can have _jitter_ in the fourth or fifth 
decimal place making it challenging to use them to create a unique `locationID`.
* Sensors are sometimes _repositioned_ in what the scientist considers the "same 
location".
* Data for a single sensor goes through different processing pipelines using
different identifiers and is later brought together as two separate timeseries.
* The spatial scale of what constitutes a "single location" depends on the 
instrumentation and scientific question being asked.
* Deriving location-based metadata from spatial datasets is computationally 
intensive unless saved and identified with a unique `locationID`.
* Automated searches for spatial metadata occasionally produce incorrect results
because of the non-infinite resolution of spatial datasets and must be manually
corrected.

## A Solution

A solution to all these problems is possible if we store spatial metadata in
simple tables in a standard directory. These tables will be referred to as 
_collections_. Location lookups can be performed with
geodesic distance calculations where a location is assigned to a pre-existing
_known location_ if it is within `distanceThreshold` meters. These will be extremely fast.

If no previously _known location_ is found, the relatively slow (seconds)
creation of a new _known location_ metadata record can be performed and then 
added to the growing collection.

For collections of stationary environmental monitors that only number in the 
thousands, an entire _collection_ can be stored as either an 
`.rda` or `.csv` file and will be under a megabyte in size making it fast to 
load. This small size also makes it possible to store multiple _known location_ 
files, each created with different locations and different distance thresholds to address 
the needs of different instruments or scientific studies.

## Immediate Advantages

Working in this manner will solve the problems initially mentioned but also 
provides further useful functionality:

* Administrators can correct entries in an individual _collection_.  (_e.g._ 
locations in river bends that even high resolution spatial datasets mis-assign)
* Additional, non-automatable metadata can be added to a _collection_. (_e.g._
commonly used location names within a community of practice)
* Different field campaigns can maintain separate _collections_.
* `.csv` or `.rda` versions of well populated tables can be downloaded from a
URL and used locally, giving scientists and analysts working with known locations 
instant access to location-specifc spatial metadata data that otherwise requires 
special skills, large datasets and many compute cycles to generate.

----

This project is supported by Mazama Science.

