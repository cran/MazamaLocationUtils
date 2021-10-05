#' @title Add to a leaflet interactive map for known locations
#'
#' @param map Leaflet map.
#' @param locationTbl Tibble of known locations.
#' @param extraVars Character vector of addition \code{locationTbl} column names
#' to be shown in leaflet popups.  
#' @param locationOnly Logical specifying whether to check for all standard
#' columns.
#' @param ... Additional arguments passed to \code{leaflet::addCircleMarker()}.
#'
#' @description This function adds interactive maps that will be displayed in
#'   RStudio's 'Viewer' tab.
#'
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'
#' @rdname table_leafletAdd
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom leaflet leaflet setView addProviderTiles addCircleMarkers

table_leafletAdd <- function(
  map = NULL,
  locationTbl = NULL,
  extraVars = NULL,
  locationOnly = FALSE,
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(map)
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = locationOnly)
  
  if ( !is.null(extraVars) ) {
    unrecognizedVars <- setdiff(extraVars, names(locationTbl))
    if ( length(unrecognizedVars) > 0 ) {
      stop("Variables in 'extraVars' not found in 'locationTbl'")
    }
  }
  
  # ----- Create popup text ----------------------------------------------------
  
  if ( locationOnly ) {
    
    popupText <- paste0(
      "longitude = ", locationTbl$longitude, ", ", "latitude = ", locationTbl$latitude, "<br>"
    )
    
  } else {
    
    # Create popupText
    popupText <- paste0(
      "<b>", locationTbl$locationName, "</b><br>",
      "locationID = ", locationTbl$locationID, "<br>",
      "longitude = ", locationTbl$longitude, ", ", "latitude = ", locationTbl$latitude, "<br>",
      "timezone = ", locationTbl$timezone, "<br>",
      "ISO = ", locationTbl$countryCode, ".", locationTbl$stateCode, "<br>",
      "county = ", locationTbl$county, "<br>",
      "address = ", locationTbl$houseNumber, ", ", locationTbl$street, ", ", locationTbl$city, ", ", 
      locationTbl$stateCode, ", ", locationTbl$zip, "<br>"
    )
    
    # Add extra vars
    for ( i in seq_along(popupText) ) {
      
      extraText <- vector("character", length(extraVars))
      for ( j in seq_along(extraVars) ) {
        var <- extraVars[j]
        extraText[j] <- paste0(var, " = ", locationTbl[i, var], "<br>")
      }
      extraText <- paste0(extraText, collapse = "")
      
      popupText[i] <- paste0(popupText[i], "<hr>", extraText)
    }
    
  }
  
  locationTbl$popupText <- popupText
  
  # ----- Create leaflet map ---------------------------------------------------
  
  m <- 
    map %>%
    leaflet::addCircleMarkers(
      lng = locationTbl$longitude,
      lat = locationTbl$latitude,
      popup = locationTbl$popupText,
      ...
    )
  
  # ----- Return ---------------------------------------------------------------
  
  return(m)
  
}
