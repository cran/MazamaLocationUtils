context("table_updateColumn")

test_that("table_updateColumn() works with no data", {
  locationTbl <- get(data("wa_monitors_500"))
  testTbl <- table_updateColumn(locationTbl, "siteName")
  expect_equal(c(names(locationTbl),"siteName"), names(testTbl))
})

test_that("table_updateColumn() works with data", {
  
  # Update records in a known location table with information obtained elsewhere
  
  locationTbl <- get(data("wa_monitors_500"))
  wa <- get(data("wa_airfire_meta"))
  
  # Record indices into for wa
  wa_indices <- seq(5,65,5)
  wa_sub <- wa[wa_indices,]
  
  # NOTE:  Some locationIDs may be NA
  locationID <- table_getLocationID(locationTbl, wa_sub$longitude, wa_sub$latitude, distanceThreshold = 1000)
  locationData <- wa_sub$siteName
  
  # Create a new column
  testTbl <- table_updateColumn(locationTbl, "siteName", locationID, locationData)
  testTbl_indices <- table_getRecordIndex(testTbl, locationID)
  
  locationTbl_siteName <- testTbl$siteName[testTbl_indices]
  wa_siteName <- wa$siteName[wa_indices]
  
  # NOTE:  locationTbl may not have every location in wa
  mask <- !is.na(locationTbl_siteName)

  expect_equal(locationTbl_siteName[mask], wa_siteName[mask])
  
})

test_that("table_updateColumn() skips unknown locations", {
  
  locationTbl <- get(data("wa_monitors_500"))
  wa <- get(data("wa_airfire_meta"))
  
  # Record indices into for wa
  wa_indices <- seq(5,65,5) 
  wa_sub <- wa[wa_indices,]
  
  # NOTE:  Include locationIDs not found in locationTbl
  locationID <- c("locationID_NOT_FOUND", table_getLocationID(locationTbl, wa_sub$longitude, wa_sub$latitude, distanceThreshold = 1000))
  locationData <- c("locationData_NOT_FOUND", wa_sub$siteName)
  
  # update an Existing column
  testTbl <- table_updateColumn(locationTbl, "locationName", locationID, locationData)
  testTbl_indices <- 
    table_getRecordIndex(testTbl, locationID) %>% 
    na.omit() %>% as.numeric()
  
  expect_true({
    all( testTbl$locationName[testTbl_indices] %in% wa$siteName )
  })
  
})

