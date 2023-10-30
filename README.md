[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/MazamaLocationUtils)](https://cran.r-project.org/package=MazamaLocationUtils)
[![Downloads](http://cranlogs.r-pkg.org/badges/MazamaLocationUtils)](https://cran.r-project.org/package=MazamaLocationUtils)
[![DOI](https://zenodo.org/badge/215816820.svg)](https://zenodo.org/badge/latestdoi/215816820)

A dedicated Slack channel has been created for announcements, support and to 
help build a community of practice around this open source package. You may 
request an invitation to join from <jonathan.callahan@dri.com>.

# MazamaLocationUtils

```
Utility functions for discovering and managing metadata associated 
with spatially unique "known locations". Applications include all 
fields of environmental monitoring (e.g. air and water quality) where 
data are collected at stationary sites.
```

## Background

This package is intended for use in data management activities
associated with fixed locations in space. The motivating fields include 
air and water quality monitoring where fixed sensors report at regular time 
intervals.

When working with environmental monitoring time series, one of the first things
you have to do is create unique identifiers for each individual time series. In 
an ideal world, each environmental time series would have both a 
`locationID` and a `deviceID` that uniquely identify the specific instrument 
making measurements and the physical location where measurements are made. A 
unique `timeseriesID` could
be produced as `locationID_deviceID`. Metadata associated with each
`timeseriesID` would contain basic information needed for downstream analysis
including at least:

`timeseriesID, locationID, deviceID, longitude, latitude, ...`

* An extended time series for an occasionally repositioned sensor would group by `deviceID`.
* Multiple sensors placed at a single location could be be grouped by `locationID`.
* Maps would be created using `longitude, latitude`.
* Time series measurements would be accessed from a secondary `data` table with 
`timeseriesID` column names.

Unfortunately, we are rarely supplied with a truly unique and truly spatial 
`locationID`. Instead we often use `deviceID` or an associated non-spatial
identifier as a stand-in for `locationID`.

Complications we have seen include:

* GPS-reported longitude and latitude can have _jitter_ in the fourth or fifth 
decimal place making it challenging to use them to create a unique `locationID`.
* Sensors are sometimes _re-positioned_ in what the scientist considers the "same 
location".
* Data from a single sensor goes through different processing pipelines using
different identifiers and is later brought together as two separate time series.
* The spatial scale of what constitutes a "single location" depends on the 
instrumentation and scientific question being asked.
* Deriving location-based metadata from spatial datasets is computationally 
intensive unless saved and identified with a unique `locationID`.
* Automated searches for spatial metadata occasionally produce incorrect results
because of the non-infinite resolution of spatial datasets and must be corrected
by hand.

## A Solution

A solution to all these problems is possible if we store spatial metadata in
simple tables in a standard directory. These tables will be referred to as 
_collections_. Location lookups can be performed with
geodesic distance calculations where a longitude-latitude pair is assigned to a pre-existing
_known location_ if it is within `distanceThreshold` meters of that location. 
These lookups will be extremely fast.

If no previously _known location_ is found, the relatively slow (seconds)
creation of a new _known location_ metadata record can be performed and then 
added to the growing collection.

For collections of stationary environmental monitors that only number in the 
thousands, this entire _collection_ can be stored as either a 
`.rda` or `.csv` file and will be under a megabyte in size making it fast to 
load. This small size also makes it possible to save multiple _collections_ 
files, each created with different locations and/or different distance thresholds
to address the needs of different scientific studies.

## Immediate Advantages

Working in this manner solves the problems initially mentioned but also 
provides further useful functionality:

* Administrators can correct entries in an individual _collection_.  (_e.g._ 
locations in river bends that even high resolution spatial datasets mis-assign)
* Additional, non-automatable metadata can be added to a _collection_. (_e.g._
commonly used location names within a community of practice)
* Different field campaigns can maintain separate _collections_.
* `.csv` or `.rda` versions of well populated tables can be downloaded from a
URL and used locally, giving scientists and analysts working with known locations 
instant access to location-specific spatial metadata data that otherwise requires 
special software and skills, large datasets and many compute cycles to generate.

----

Development of this R package has been supported with funding from the 
following institutions:

* USFS [AirFire Research Team](https://www.airfire.org)

Questions regarding further development of the package should be directed to 
<jonathan.callahan@dri.edu>.
