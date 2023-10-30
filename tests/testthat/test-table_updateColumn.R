context("table_updateColumn")

test_that("table_updateColumn() works with no data", {
  locationTbl <- get(data("wa_monitors_500"))
  testTbl <- table_updateColumn(locationTbl, "AQSID")
  expect_equal(c(names(locationTbl),"AQSID"), names(testTbl))
})

test_that("table_updateColumn() works with data", {
  
  # Update records in a known location table with information obtained elsewhere
  
  locationTbl <- get(data("wa_monitors_500"))
  airfire <- get(data("wa_airfire_meta"))
  
  # Record indices for airfire
  airfire_indices <- seq(5,65,5)
  airfire_sub <- airfire[airfire_indices,]
  
  # Get matching locations in locationTbl
  # NOTE:  Some locationIDs may be NA
  locationID <- table_getLocationID(locationTbl, airfire_sub$longitude, airfire_sub$latitude, distanceThreshold = 1000)
  locationData <- airfire_sub$AQSID
  
  # Create a new column
  testTbl <- table_updateColumn(locationTbl, "AQSID", locationID, locationData)
  
  testTbl_indices <- table_getRecordIndex(testTbl, locationID)
  
  locationTbl_AQSID <- testTbl$AQSID[testTbl_indices]
  airfire_AQSID <- airfire$AQSID[airfire_indices]
  
  # NOTE:  locationTbl may not have every location in airfire
  mask <- !is.na(locationTbl_AQSID)

  expect_equal(locationTbl_AQSID[mask], airfire_AQSID[mask])
  
})

test_that("table_updateColumn() skips unknown locations", {
  
  locationTbl <- get(data("wa_monitors_500"))
  airfire <- get(data("wa_airfire_meta"))
  
  # Record indices into for airfire
  airfire_indices <- seq(5,65,5) 
  airfire_sub <- airfire[airfire_indices,]
  
  # NOTE:  Include locationIDs not found in locationTbl
  locationID <- c("locationID_NOT_FOUND", table_getLocationID(locationTbl, airfire_sub$longitude, airfire_sub$latitude, distanceThreshold = 1000))
  locationData <- c("locationData_NOT_FOUND", airfire_sub$AQSID)
  
  # update an Existing column
  testTbl <- table_updateColumn(locationTbl, "locationName", locationID, locationData)
  testTbl_indices <- 
    table_getRecordIndex(testTbl, locationID) %>% 
    na.omit() %>% as.numeric()
  
  expect_true({
    all( testTbl$locationName[testTbl_indices] %in% airfire$AQSID )
  })
  
})

