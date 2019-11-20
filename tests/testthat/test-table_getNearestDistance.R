context("table_getNearestDistance")

test_that("table_getNearestDistance() works", {
  locationTbl <- get(data("wa_monitors_500"))
  
  # Wenatchee
  lon <- -120.325278
  lat <- 47.423333
  
  testClose <- table_getNearestDistance(locationTbl, lon, lat, radius = 50)
  testFar <- table_getNearestDistance(locationTbl, lon, lat, radius = 5000)
  expect_equal(testClose, as.numeric(NA))
  expect_equal(testFar, 1495, tolerance = 1.0)
})

