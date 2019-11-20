context("table_getNearestLocation")

test_that("table_getNearestLocation() works", {
  locationTbl <- get(data("wa_monitors_500"))
  
  emptyRecord <- table_initialize()    # zero rows
  emptyRecord[1,1] <- as.character(NA) # one row, all NA
  wenatcheeRecord <- locationTbl %>% dplyr::filter(city == "Wenatchee")
  
  # Wenatchee
  lon <- -120.325278
  lat <- 47.423333
  
  testClose <- table_getNearestLocation(locationTbl, lon, lat, radius = 50)
  testFar <- table_getNearestLocation(locationTbl, lon, lat, radius = 5000)
  expect_equal(testClose, emptyRecord)
  expect_equal(testFar, wenatcheeRecord)
})

