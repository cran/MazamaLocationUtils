# MazamaLocationUtils 0.1.13

* Updated to require **geodist** 0.0.7.

# MazamaLocationUtils 0.1.12

* Updated `location_getSingleAddress_Photon()` to remove **revgeo** dependency.
* Updated to require **geodist** 0.0.6.007 to handle errors finding longitude
and latitude columns in the passed in tibble.
* `geodist::geodist()` is now always called with `measure = "geodesic"` to avoid
warning messages from `geodist()` about inaccuracies with `measure = "cheap"`
(the default).
* `mazama_initialize()` now installs required datasets if they are missing.
* Updated to require **MazamaSpatialUtils** 0.7.

# MazamaLocationUtils 0.1.11

* Added unit test for `table_findOverlappingLocations()`.

# MazamaLocationUtils 0.1.10

* Added `table_initializeExisting()` for fast conversion of an existing 
table of spatial metadata into a standardized "known location" table.
* Added `table_findOverlappingLocations()` to help choose an appropriate radius
when initializing frorm an existing metadata table.
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

* Massive refatoring of function names.
* Example datasets and unit tests.

# MazamaLocationUtils 0.1.1

* Removed `getLoations()`
* Added `getLocationID()` and `getNearestLocation()`
* Various cleanup/refactor

# MazamaLocationUtils 0.1.0

* Initial Release
