#' @title Leaflet interactive map for known locations
#'
#' @param locationTbl Tibble of known locations.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.
#' @param extraVars Character vector of addition \code{locationTbl} column names
#' to be shown in leaflet popups.  
#' @param jitter Amount to use to slightly adjust locations so that multiple
#' monitors at the same location can be seen. Use zero or \code{NA} to see
#' precise locations.
#' @param ... Additional arguments passed to \code{leaflet::addCircleMarker()}.
#'
#' @description This function creates interactive maps that will be displayed in
#' RStudio's 'Viewer' tab. The default setting of `jitter` will move locations
#' randomly within an ~50 meter radius so that overlapping locations can be 
#' identified. Set `jitter = 0` to see precise locations.
#'
#' @details The \code{maptype} argument is mapped onto leaflet "ProviderTile"
#'   names. Current mappings include:
#' \enumerate{
#' \item{"roadmap"}{ -- "OpenStreetMap"}
#' \item{"satellite"}{ -- "Esri.WorldImagery"}
#' \item{"terrain"}{ -- "Esri.WorldTopoMap"}
#' \item{"toner"}{ -- "Stamen.Toner"}
#' }
#'
#' If a character string not listed above is provided, it will be used as the
#' underlying map tile if available. See
#' \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
#' "provider tiles" to use as the background map.
#' 
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'
#' @rdname table_leaflet
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom rlang .data
#' @importFrom leaflet leaflet fitBounds addProviderTiles addCircleMarkers
#' 
#' @examples
#' \dontrun{
#' library(MazamaLocationUtils)
#' 
#' # A table with all core metadata
#' table_leaflet(wa_monitors_500)
#'   
#' # A table missing some core metadata
#' table_leaflet(
#'   wa_airfire_meta,
#'   extraVars = c("stateCode", "countyName", "msaName")
#' )
#' 
#' # Customizing the map
#' table_leaflet(
#'   wa_airfire_meta,
#'   extraVars = c("stateCode", "countyName", "msaName"),
#'   radius = 6,
#'   color = "black",
#'   weight = 2,
#'   fillColor = "red",
#'   fillOpacity = 0.3
#' )
#' }

table_leaflet <- function(
  locationTbl = NULL,
  maptype = c("terrain", "roadmap", "satellite", "toner"),
  extraVars = NULL,
  jitter = 5e-4,
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  maptype <- match.arg(maptype)
  
  if ( !is.null(extraVars) ) {
    unrecognizedVars <- setdiff(extraVars, names(locationTbl))
    if ( length(unrecognizedVars) > 0 ) {
      stop("Variables in 'extraVars' not found in 'locationTbl'")
    }
  }
  
  # Filter out missing location data
  locationTbl <-
    locationTbl %>%
    dplyr::filter(!is.na(.data$latitude)) %>%
    dplyr::filter(!is.na(.data$longitude))
  
  hasCoreMetadata <- all(coreMetadataNames %in% names(locationTbl))
  
  if ( is.null(jitter) || is.na(jitter) ) {
    jitter <- 0
  }

  # * argsList -----
  
  argsList <- list(...)
  
  argsList$lng <- jitter(locationTbl$longitude, amount = jitter)
  argsList$lat <- jitter(locationTbl$latitude, amount = jitter)
  
  # ----- Create base map ------------------------------------------------------
  
  # * providerTiles -----
  
  # Convert maptype to a character string that addProviderTiles can read
  if ( missing(maptype) || maptype == 'terrain') {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  } else {
    providerTiles <- maptype
  }
  
  # * zoom -----
  
  # Extract view information
  lonRange <- range(locationTbl$longitude, na.rm = TRUE)
  latRange <- range(locationTbl$latitude, na.rm = TRUE)
  
  # * base map -----
  
  argsList$map <- 
    leaflet::leaflet(locationTbl) %>%
    leaflet::fitBounds(lonRange[1], latRange[1], lonRange[2], latRange[2]) %>%
    leaflet::addProviderTiles(providerTiles)
  
  # ----- Add circle markers ---------------------------------------------------
  
  # * weight -----
  
  if ( !"weight" %in% argsList ) 
    argsList$weight <- 1
  
  # * popup text -----
  
  # Initialize empty popupText
  popupText <- vector("character", nrow(locationTbl))
  
  # Create coreText
  
  if ( hasCoreMetadata ) {
    
    # Use guaranteed fields
    coreText <- paste0(
      "<b>", locationTbl$locationName, "</b><br>",
      "locationID = ", locationTbl$locationID, "<br>",
      "longitude = ", locationTbl$longitude, ", ", "latitude = ", locationTbl$latitude, "<br>",
      "timezone = ", locationTbl$timezone, "<br>",
      "ISO = ", locationTbl$countryCode, ".", locationTbl$stateCode, "<br>",
      "county = ", locationTbl$countyName, "<br>",
      "addr. = ", locationTbl$houseNumber, ", ", locationTbl$street, ", ", locationTbl$city, ", ", 
      locationTbl$stateCode, ", ", locationTbl$zip, "<br>"
    )
    
    
  } else {
    
    # Use reasonable best guesses to create 3 lines of core metadata
    
    tblNames <- names(locationTbl)
    
    # Bold location identifier at the top (in preference order)
    if ( "locationName" %in% tblNames ) {
      coreText_1 <-  paste0("<b>", locationTbl$locationName, "</b><br>")
    } else if ( "siteName" %in% tblNames ) {
      coreText_1 <-  paste0("<b>", locationTbl$siteName, "</b><br>")
    } else if ( "AQSID" %in% tblNames ) {
      coreText_1 <- paste0("<b>", locationTbl$AQSID, "</b><br>")
    } else if ( "aqsID" %in% tblNames ) {
      coreText_1 <- paste0("<b>", locationTbl$aqsID, "</b><br>")
    } else if ( "aqsid" %in% tblNames ) {
      coreText_1 <- paste0("<b>", locationTbl$aqsid, "</b><br>")
    } else {
      coreText_1 <- ""
    }
    
    # US EPA AQSID if not already found
    if ( "AQSID" %in% tblNames ) {
      coreText_2 <-  paste0("AQSID = ", locationTbl$AQSID, "<br>")
    } else if ( "aqsID" %in% tblNames ) {
      coreText_2 <-  paste0("AQSID = ", locationTbl$aqsID, "<br>")
    } else if ( "aqsid" %in% tblNames ) {
      coreText_2 <-  paste0("AQSID = ", locationTbl$aqsid, "<br>")
    } else {
      coreText_2 <- ""
    }

    # Location
    coreText_3 <- paste0(
      "longitude = ", locationTbl$longitude, ", ", "latitude = ", locationTbl$latitude, "<br>"
    )
    
    # Paste them together
    coreText <- paste0(coreText_1, coreText_2, coreText_3)
    
  }
  
  # Add extra vars
  for ( i in seq_along(popupText) ) {
    
    extraText <- vector("character", length(extraVars))
    for ( j in seq_along(extraVars) ) {
      var <- extraVars[j]
      extraText[j] <- paste0(var, " = ", locationTbl[i, var], "<br>")
    }
    extraText <- paste0(extraText, collapse = "")
    
    popupText[i] <- paste0(coreText[i], "<hr>", extraText)
    
  }
  
  locationTbl$popupText <- popupText
  
  argsList$popup <- popupText
  
  # * add markers -----
  
  map <- do.call(leaflet::addCircleMarkers, argsList)
  
  # ----- Return ---------------------------------------------------------------
  
  return(map)
  
}
