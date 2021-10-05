context("table_updateColumn")

test_that("table_updateColumn() works with no data", {
  locationTbl <- get(data("wa_monitors_500"))
  testTbl <- table_updateColumn(locationTbl, "siteName")
  expect_equal(c(names(locationTbl),"siteName"), names(testTbl))
})

test_that("table_updateColumn() works with data", {
  locationTbl <- get(data("wa_monitors_500"))
  wa <- get(data("wa_airfire_meta"))
  
  # Reord indices into for wa
  wa_indices <- seq(5,65,5)
  wa_sub <- wa[wa_indices,]
  
  locationID <- table_getLocationID(locationTbl, wa_sub$longitude, wa_sub$latitude, distanceThreshold = 1000)
  locationData <- wa_sub$siteName
  
  testTbl <- table_updateColumn(locationTbl, "siteName", locationID, locationData)
  
  # Reord indices for testTbl
  testTbl_indices <- table_getRecordIndex(locationTbl, locationID)
  
  expect_equal(testTbl$siteName[testTbl_indices], wa$siteName[wa_indices])
})

