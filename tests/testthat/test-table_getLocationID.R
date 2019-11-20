context("table_getLocationID")

test_that("correct locationID is returned", {
  locationTbl <- get(data("wa_monitors_500"))
  
  locationID <- wa_monitors_500$locationID
  longitude <- wa_monitors_500$longitude
  latitude <- wa_monitors_500$latitude
  
  testID <- table_getLocationID(locationTbl, longitude, latitude, radius = 500)
  expect_equal(locationID, testID)
})

test_that("radius works", {
  locationTbl <- get(data("wa_monitors_500"))
  
  locationID <- wa_monitors_500$locationID
  longitude <- wa_monitors_500$longitude + 0.01 # ~ 780 m east
  latitude <- wa_monitors_500$latitude + 0.01 # ~ 780 m north
  allNA <- rep(as.character(NA), length(locationID))
  
  testID <- table_getLocationID(locationTbl, longitude, latitude, radius = 500)
  expect_equal(allNA, testID)
  
  testID <- table_getLocationID(locationTbl, longitude, latitude, radius = 5000)
  expect_equal(locationID, testID)
})
