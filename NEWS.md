# MazamaLocationUtils 0.4.4

* Fixed bug in `clusterByDistance()` which could fail when incoming tibbles
had very few rows.

# MazamaLocationUtils 0.4.3

* Updated `location_getSingleElevation_USGS()` to use new USGS API.

# MazamaLocationUtils 0.4.2

* Add uniqueness of `locationID` to `validateLocationTable()`.
* `table_save()` now supports a vector of `outputType` formats and will save
the location table in multiple formats.

# MazamaLocationUtils 0.4.1

* Documentation fixes for CRAN submission.

# MazamaLocationUtils 0.4.0

* Added `clusterByDistance()` and `table_addClustering()` functions.
* Renamed `zip` to `postalCode` in the core metadata.
* `location_createID()` now defaults to the `"geohash"` algorithm.
* Renamed `mazama_initialize()` to `initializeMazamaSpatialUtils()`.
* Updated example data to October, 2023.
* Added "Developer Style Guide" vignette.
* Improved documentation.

# MazamaLocationUtils 0.3.11

* Added `algorithm argument` to location_createID() to select between "digest"
and "geohash".
* Addressed CRAN package documentation issue.

# MazamaLocationUtils 0.3.10

* Added `na.rm = TRUE` to all calls to `any(...)` in various `table_~()` functions.

# MazamaLocationUtils 0.3.9

* Updated `location_getSingleElevation_USGS()` to use the new web service at
https://apps.nationalmap.gov/pqs/.

# MazamaLocationUtils 0.3.8

* Updated documentation in `location_getSingleElevation_USGS()`.

# MazamaLocationUtils 0.3.7

* Updated `table_updateColumn()` to ignore incoming `locationIDs` that are not 
found in the `locationTbl`.

# MazamaLocationUtils 0.3.6

* Added `table_getDistanceFromTarget()` and edited the description of other
distance functions to make clear what each does.

# MazamaLocationUtils 0.3.5

* Removed `validateMazamaSpatialUtils()` test from `table_addCoreMetadata()` as
this function just adds empty columns and does not need any spatial data.

# MazamaLocationUtils 0.3.4

* Updated `table_addOpenCageInfo()` to handle address components that are
missing from the openCage results.

# MazamaLocationUtils 0.3.3

* Updated `table_findAdjacentLocations()` to only only return unique locations
and to ensure they are returned in order of separation distance.
* Corrected behavior when using `jitter = 0` in `table_leaflet()` and 
`table_leafletAdd()`.
* Added `table_filterByDistance()`.

# MazamaLocationUtils 0.3.2

* Ensure that address components are of class "character" throughout the code.
* Added `location_getOpenCageInfo()` to get OpenCage information for a single location.

# MazamaLocationUtils 0.3.1

* Updated `location_getCensusBlock()` to add `censusYear` argument.
* Minor documentation improvements.

# MazamaLocationUtils 0.3.0

* Added `table_addOpenCageInfo()` with support for reverse geocoding using 
**tidygeocoder**.
* `mazama_initialize()` now only loads the high resolution datasets used in
`location_initialize()`.

# MazamaLocationUtils 0.2.9

* Function signatures now provide multiple options where appropriate and these
are checked with `match.arg()`.
* Updated `table_load()` looks for .rda and then .csv versions.
* Updated `table_save()` to avoid deprecated arguments to `readr::write_csv()`.

# MazamaLocationUtils 0.2.8

* Removed unused dependency on **digest**.

# MazamaLocationUtils 0.2.7

* All examples now meet the CRAN directive to avoid stopping if a web 
resource is unavailable.
* Improved error messages in `table_load()` and `table_save()`.
* Removed `table_export()`. Use `table_save()` instead.

# MazamaLocationUtils 0.2.6

* Removed `~APIKey()` functionality. Now importing this from 
`MazamaCoreUtils` 0.4.10.
* Optimized `table_getLocationID()` and `table_getNearestDistance()` by only 
calculating distance for unique locations. This helps tremendously when 
`longitude` and `latitude` come from "tidy" dataframes.

# MazamaLocationUtils 0.2.5

* Fix timezone assignment bug in `table_initializeExisting()`.

# MazamaLocationUtils 0.2.4

* Improved documentation.

# MazamaLocationUtils 0.2.3

* Refactored `table_leaflet()` and `table_leafletAdd()` with improved defaults
and more flexibility.

# MazamaLocationUtils 0.2.2

* Rename `county` to `countyName`. (This change more closely matches spatial
metadata found in other systems and fits with the pattern of `~Code`/`~Name` 
pairs in **MazamaSpatialUtils** as is the case with `countryCode/CountryName` 
and `stateCode/stateName`.)
* Regenerated example datasets.
* Updated tests to reflect regenerated example datasets.
* Fixed bug in `table_findAdjacentDistances()` when only two locations are 
adjacent.
* Re-exporting location validation functions from **MazamaCoreUtils**.
* `table_updateColumn()` now ignores `NA` values in `locationID`.

# MazamaLocationUtils 0.2.1

* Using **MazamaCoreUtils** version of `validateLonLat()`, `validateLonsLats()`,
`createLocationID()`.
* Removed dependency on **sp** package.

# MazamaLocationUtils 0.2.0

Version 0.2.x focuses on usability improvements after initial work with the package.

* **Renamed `radius` to `distanceThreshold` throughout for clarity.**
* Updated `table_initializeExisting()` to only perform spatial searches where
data are missing in the incoming table. This greatly speeds up performance.
* New `table_leafletAdd()` function to make it easier to compare "known locations"
tables."
* New `table_addCoreMetadata()` function adds columns of `NA` values for
any missing core metadata but does not perform any spatial calculations.
* New `table_findAdjacentLocations()` function returns a tibble of all locations
that are too close (_i.e._ separated by < `distanceThreshold`).
* `table_findOverlappingLocations()` renamed to `table_findAdjacentDistances()`.
* Added `measure` argument to `table_findAdjacentLocations()`, 
`table_findAdjacentDistances()`, `table_getLocationID()` and `table_getNearestDistance()`.
* Added `na.rm` argument to `validateLonsLats()`.
* Improved validation of arguments in all `table_~()` functions.
* Update function arguments to consistently use `locationTbl` whenever an
incoming table includes `longitude` and `latitude` variables.
* New `table_leaflet()` function to display locations and metadata.

# MazamaLocationUtils 0.1.13

* Updated to require **geodist** 0.0.7.

# MazamaLocationUtils 0.1.12

* Updated `location_getSingleAddress_Photon()` to remove **revgeo** dependency.
* Updated to require **geodist** 0.0.6.007 to handle errors finding longitude
and latitude columns in the passed in tibble.
* `geodist::geodist()` is now always called with `measure = "geodesic"` to avoid
warning messages from `geodist()` about inaccuracies with `measure = "cheap"`
(the `geodist()` default).
* `mazama_initialize()` now installs required datasets if they are missing.
* Updated to require **MazamaSpatialUtils** 0.7.

# MazamaLocationUtils 0.1.11

* Added unit test for `table_findOverlappingLocations()`.

# MazamaLocationUtils 0.1.10

* Added `table_initializeExisting()` for fast conversion of an existing 
table of spatial metadata into a standardized "known location" table.
* Added `table_findOverlappingLocations()` to help choose an appropriate radius
when initializing from an existing metadata table.
* Added `addressService` argument to `table_addLocation()`, 
`table_addSingleLocation() and `location_initialize()` to skip the address
step that requires web services.

# MazamaLocationUtils 0.1.9

* Documentation tweaks.

# MazamaLocationUtils 0.1.8

* New `location_getCensusBlock()` function.
* New `location_getSingleAddress_TexasAM()` function.

# MazamaLocationUtils 0.1.7

* Added support for API keys with `setAPIKey()` and `getAPIKey()`.

# MazamaLocationUtils 0.1.6

* Updated checks and explicit instructions for installing required spatial data.

# MazamaLocationUtils 0.1.5

* Changed examples from \code{\dontrun} to \code{\donttest} per CRAN suggestion.

# MazamaLocationUtils 0.1.4

* Corrections to introductory vignette.

# MazamaLocationUtils 0.1.3

* Added introductory vignette.

# MazamaLocationUtils 0.1.2

* Massive refactoring of function names.
* Example datasets and unit tests.

# MazamaLocationUtils 0.1.1

* Removed `getLoations()`
* Added `getLocationID()` and `getNearestLocation()`
* Various cleanup/refactor

# MazamaLocationUtils 0.1.0

* Initial Release
