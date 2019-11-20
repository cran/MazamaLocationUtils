context("table_getRecordIndex")

test_that("correct index is returned", {
  locationTbl <- get(data("wa_monitors_500"))
  
  indices <- c(5,10,15)
  locationID <- wa_monitors_500$locationID[indices]

  testIndices <- table_getRecordIndex(locationTbl, locationID)
  expect_equal(indices, testIndices)
})

test_that("NA is returned", {
  locationTbl <- get(data("wa_monitors_500"))
  
  indices <- c(5,10,15)
  locationID <- c(wa_monitors_500$locationID[indices], "Rumplestiltskin")
  
  testIndices <- table_getRecordIndex(locationTbl, locationID)
  expect_equal(c(indices, NA), testIndices)
})
